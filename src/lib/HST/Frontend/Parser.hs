{-# LANGUAGE FlexibleInstances #-}

-- | This module contains functions for parsing Haskell modules and expressions
--   with the different front ends.
module HST.Frontend.Parser ( Parsable(parseModule, parseExp) ) where

import qualified Bag                                        as GHC
import           Control.Monad                              ( unless )
import           Data.List                                  ( intercalate )
import qualified ErrUtils                                   as GHC
import qualified Language.Haskell.Exts                      as HSE
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as GHC
import qualified Lexer                                      as GHC
import qualified Outputable                                 as GHC
import           Polysemy
  ( Member, Members, Sem )

import           HST.Effect.Cancel                          ( Cancel, cancel )
import           HST.Effect.Report
  ( Report, report, reportFatal )
import           HST.Frontend.GHC.Config
  ( GHC, defaultDynFlags )
import           HST.Frontend.HSE.Config                    ( HSE )
import qualified HST.Frontend.Syntax                        as S
import           HST.Frontend.Transformer
  ( ExpType(ExpHSE, ExpGHC), ModuleType(ModuleHSE, ModuleGHC), Transformable(..) )
import           HST.Util.Messages
  ( Severity(Error, Warning), messageWithoutSrcSpan )

-- | Type class for "HST.Frontend.Syntax" configurations for which 'S.Module's
--   and 'S.Exp'ressions can be parsed.
--
--   Modules are always transformed into our internal representation after
--   parsing.
class Transformable a => Parsable a where
  -- | Parses the given Haskell file.
  --
  --   Syntax errors are reported. The computation can be canceled even if
  --   there is no fatal error.
  parseModule :: Members '[Report, Cancel] r
              => FilePath -- ^ The name of the input file.
              -> String   -- ^ The contents of the input file.
              -> Sem r (S.Module a)

  -- | Parses the given Haskell expression.
  --
  --   Syntax errors are reported. The computation can be canceled even if
  --   there is no fatal error.
  parseExp :: Members '[Report, Cancel] r => String -> Sem r (S.Exp a)

-- | Parses a Haskell module or expression with the parser of
--   @haskell-src-exts@.
instance Parsable (HSE HSE.SrcSpanInfo) where
  parseModule inputFilename input = transformModule . ModuleHSE
    =<< handleParseResultHSE (HSE.parseModuleWithMode parseMode input)
   where
    -- | Configuration of the @haskell-src-exts@ parser.
    parseMode :: HSE.ParseMode
    parseMode = HSE.defaultParseMode { HSE.parseFilename = inputFilename }

  parseExp input = transformExp . ExpHSE
    =<< handleParseResultHSE (HSE.parseExp input)

-- | Handles a parse result of the HSE front end, i. e. returns the AST node
--   contained in the parse result or reports an error if parsing failed.
handleParseResultHSE :: Member Report r => HSE.ParseResult a -> Sem r a
handleParseResultHSE (HSE.ParseOk astNode)        = return astNode
handleParseResultHSE (HSE.ParseFailed srcLoc msg) = reportFatal
  $ messageWithoutSrcSpan Error
  $ msg
  ++ " in "
  ++ HSE.srcFilename srcLoc
  ++ ":"
  ++ show (HSE.srcLine srcLoc)
  ++ ":"
  ++ show (HSE.srcColumn srcLoc)
  ++ "."

-- | Parses a Haskell module or expression with the parser of @ghc-lib-parser@.
instance Parsable GHC where
  parseModule inputFilename input = transformModule . ModuleGHC
    =<< handleParseResultGHC (GHC.parseFile inputFilename defaultDynFlags input)

  parseExp input = transformExp . ExpGHC
    =<< handleParseResultGHC (GHC.parseExpression input defaultDynFlags)

-- | Handles a parse result of the GHC front end, i. e. returns the AST node
--   contained in the parse result, if available, and reports all errors and
--   warnings that were reported during parsing.
handleParseResultGHC
  :: Members '[Cancel, Report] r => GHC.ParseResult a -> Sem r a
handleParseResultGHC parseResult = case parseResult of
  GHC.POk state astNode -> do
    reportParsingMessages state
    return astNode
  GHC.PFailed state     -> do
    reportParsingMessages state
    cancel
 where
  -- | Reports all errors and warnings that were reported during parsing.
  --
  --   Cancels the computation if there is a parsing error. There can be
  --   parsing errors even if 'GHC.parseFile' returns 'GHC.POk' (e.g., if
  --   language extensions are needed such that the parsed AST represents
  --   a valid module).
  reportParsingMessages
    :: Members '[Cancel, Report] r => GHC.PState -> Sem r ()
  reportParsingMessages state = do
    let (warnings, errors) = GHC.getMessages state defaultDynFlags
    GHC.mapBagM_ (reportErrMsg Warning) warnings
    GHC.mapBagM_ (reportErrMsg Error) errors
    unless (GHC.isEmptyBag errors) cancel

  -- | Reports an error message or warning from @ghc-lib-parser@.
  reportErrMsg :: Member Report r => Severity -> GHC.ErrMsg -> Sem r ()
  reportErrMsg severity msg = report
    $ messageWithoutSrcSpan severity
    $ intercalate "\n • "
    $ map (GHC.showSDoc defaultDynFlags)
    $ GHC.errDocImportant
    $ GHC.errMsgDoc msg
