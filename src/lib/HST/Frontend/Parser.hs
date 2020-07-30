{-# LANGUAGE PackageImports, TypeFamilies #-}

-- | This module contains functions for parsing Haskell modules with the
--   different front ends.

module HST.Frontend.Parser
  ( Parsable(parseModule)
  , ParsedModule(ParsedModuleHSE, ParsedModuleGHC)
  , getParsedModuleHSE
  , getParsedModuleGHC
  )
where

import           Control.Monad                  ( unless )
import           Data.List                      ( intercalate )
import qualified "ghc-lib-parser" Bag          as GHC
import qualified "ghc-lib-parser" ErrUtils     as GHC
import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" Lexer        as GHC
import qualified "ghc-lib-parser" Outputable   as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Parser
                                               as GHC
import qualified Language.Haskell.Exts         as HSE
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           HST.Effect.Report              ( Message(Message)
                                                , Report
                                                , Severity(Error, Warning)
                                                , report
                                                , reportFatal
                                                )
import           HST.Effect.Cancel              ( Cancel
                                                , cancel
                                                )
import           HST.Frontend.FromGHC           ( GHC
                                                , defaultDynFlags
                                                )
import           HST.Frontend.FromHSE           ( HSE )

-- | Type class for "HST.Frontend.Syntax" configurations for which 'S.Module's
--   can be parsed.
class Parsable a where

  -- | Type family for the return type of 'parseModule'.
  data ParsedModule a :: *

  -- | Parses the given Haskell file.
  --
  --   Syntax errors are reported. The computation can be canceled even if
  --   there is no fatal error.
  parseModule
    :: Members '[Report, Cancel] r
    => FilePath -- ^ The name of the input file.
    -> String   -- ^ The contents of the input file.
    -> Sem r (ParsedModule a)

-- | Parses a Haskell module with the parser of @haskell-src-exts@.
instance Parsable HSE where
  data ParsedModule HSE
    = ParsedModuleHSE { getParsedModuleHSE :: HSE.Module HSE.SrcSpanInfo }

  parseModule inputFilename input =
    case HSE.parseModuleWithMode parseMode input of
      HSE.ParseOk inputModule -> return (ParsedModuleHSE inputModule)
      HSE.ParseFailed srcLoc msg ->
        reportFatal
          $  Message Error
          $  msg
          ++ " in "
          ++ HSE.srcFilename srcLoc
          ++ ":"
          ++ show (HSE.srcLine srcLoc)
          ++ ":"
          ++ show (HSE.srcColumn srcLoc)
          ++ "."
   where
     -- | Configuration of the @haskell-src-exts@ parser.
    parseMode :: HSE.ParseMode
    parseMode = HSE.defaultParseMode { HSE.parseFilename = inputFilename }

-- | Parses a Haskell module with the parser of @ghc-lib-parser@.
instance Parsable GHC where
  data ParsedModule GHC
    = ParsedModuleGHC { getParsedModuleGHC :: GHC.HsModule GHC.GhcPs }

  parseModule inputFilename input =
    case GHC.parseFile inputFilename defaultDynFlags input of
      GHC.POk state inputModule -> do
        reportParsingMessages state
        return (ParsedModuleGHC (GHC.unLoc inputModule))
      GHC.PFailed state -> do
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
    reportErrMsg severity msg =
      report
        $ Message severity
        $ intercalate "\n • "
        $ map (GHC.showSDoc defaultDynFlags)
        $ GHC.errDocImportant
        $ GHC.errMsgDoc msg
