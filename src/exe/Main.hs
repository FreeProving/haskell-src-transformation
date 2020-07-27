{-# LANGUAGE PackageImports #-}

-- | This module contains the command line interface for the
--   @haskell-src-transformations@ package.

module Main
  ( main
  )
where

import           Control.Exception              ( SomeException
                                                , displayException
                                                )
import           Control.Monad                  ( unless )
import           Data.List                      ( intercalate )
import           Data.List.Extra                ( splitOn )
import qualified "ghc-lib-parser" Bag          as GHC
import qualified "ghc-lib-parser" DynFlags     as GHC
import qualified "ghc-lib-parser" ErrUtils     as GHC
import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" Lexer        as GHC
import qualified "ghc-lib-parser" Outputable   as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Parser
                                               as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config
                                               as GHC
import qualified Language.Haskell.Exts         as HSE
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )
import           Polysemy.Embed                 ( Embed
                                                , embed
                                                )
import           Polysemy.Final                 ( embedToFinal
                                                , runFinal
                                                )
import           System.Console.GetOpt          ( usageInfo )
import           System.Environment             ( getProgName )
import           System.Directory               ( createDirectoryIfMissing )
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                , joinPath
                                                , takeBaseName
                                                , takeDirectory
                                                )
import           System.IO                      ( stderr )

import           HST.Application                ( processModule )
import           HST.Effect.Report              ( Message(Message)
                                                , Report
                                                , Severity
                                                  ( Error
                                                  , Debug
                                                  , Internal
                                                  , Warning
                                                  )
                                                , exceptionToReport
                                                , filterReportedMessages
                                                , msgSeverity
                                                , report
                                                , reportFatal
                                                , reportToHandleOrCancel
                                                )
import           HST.Effect.Cancel              ( Cancel
                                                , cancel
                                                , cancelToExit
                                                )
import           HST.Effect.Env                 ( runEnv )
import           HST.Effect.Fresh               ( runFresh )
import           HST.Effect.GetOpt              ( GetOpt
                                                , getOpt
                                                , runWithArgsIO
                                                )
import qualified HST.Frontend.FromHSE          as FromHSE
import qualified HST.Frontend.FromGHC          as FromGHC
import qualified HST.Frontend.Syntax           as S
import qualified HST.Frontend.ToHSE            as ToHSE
import qualified HST.Frontend.ToGHC            as ToGHC
import           HST.Options                    ( Frontend(..)
                                                , optShowHelp
                                                , optInputFiles
                                                , optOutputDir
                                                , optEnableDebug
                                                , optFrontend
                                                , optionDescriptors
                                                , parseFrontend
                                                )

-------------------------------------------------------------------------------
-- Usage Information                                                         --
-------------------------------------------------------------------------------

-- | The header of the help message.
--
--   This text is added before the description of the command line arguments.
usageHeader :: FilePath -> String
usageHeader progName =
  "Usage: "
    ++ progName
    ++ " [options...] <input-files...>\n\n"
    ++ "Command line options:"

-- | Prints the help message for the command line interface.
--
--   The help message is displayed when the user specifies the @--help@ option.
putUsageInfo :: IO ()
putUsageInfo = do
  progName <- getProgName
  putStrLn (usageInfo (usageHeader progName) optionDescriptors)

-------------------------------------------------------------------------------
-- Main                                                                      --
-------------------------------------------------------------------------------

-- | The main function of the command line interface.
--
--   Runs the 'application' and interprets all unhandled effects.
main :: IO ()
main =
  runFinal
    . embedToFinal
    . cancelToExit
    . reportToHandleOrCancel stderr
    . exceptionToReport exceptionToMessage
    . runWithArgsIO
    $ application
 where
  exceptionToMessage :: SomeException -> Message
  exceptionToMessage e = Message Internal (displayException e)

-- | The main computation of the command line interface.
--
--   Parses the command line arguments and input file. The transformation is
--   applied on the parsed input module and a state constructed from the
--   command line arguments. The output is either printed to the console
--   or a file.
application :: Members '[Cancel, Embed IO, GetOpt, Report] r => Sem r ()
application = do
  -- Filter reported message based on @--debug@ flag.
  debuggingEnabled <- getOpt optEnableDebug
  filterReportedMessages (\msg -> debuggingEnabled || msgSeverity msg /= Debug)
    $ do
        -- Show usage information when the @--help@ flag is specified or there
        -- is no input file.
        showHelp   <- getOpt optShowHelp
        inputFiles <- getOpt optInputFiles
        if showHelp || null inputFiles
          then embed putUsageInfo
          else mapM_ processInputFile inputFiles

-------------------------------------------------------------------------------
-- Pattern Matching Compilation                                              --
-------------------------------------------------------------------------------

-- | Applies the transformation to the given file and writes the transformed
--   module to the console or an output file depending on the command line
--   options.
--
--   The name of the output file is generated by appending the result of
--   'makeOutputFileName' for the input module to the output directory.
--   If the output directory does not exist, the output directory and all of
--   its parent directories are created.
processInputFile
  :: Members '[Cancel, Embed IO, GetOpt, Report] r => FilePath -> Sem r ()
processInputFile inputFile = do
  input                <- embed $ readFile inputFile
  frontendString       <- getOpt optFrontend
  frontend             <- parseFrontend frontendString
  (output, moduleName) <- processInput frontend inputFile input
  maybeOutputDir       <- getOpt optOutputDir
  case maybeOutputDir of
    Just outputDir -> do
      let outputFile = outputDir </> makeOutputFileName inputFile moduleName
      embed $ createDirectoryIfMissing True (takeDirectory outputFile)
      embed $ writeFile outputFile output
    Nothing -> embed $ putStrLn output

-- | Parses a given string to a module using the given front end, then applies
--   the transformation and at last returns the module name and a pretty
--   printed version of the transformed module.
processInput
  :: Members '[Cancel, GetOpt, Report] r
  => Frontend -- ^ The frontend to use to parse and print the file.
  -> FilePath -- ^ The name of the input file.
  -> String   -- ^ The contents of the input file.
  -> Sem r (String, Maybe String)
processInput HSE    = processInputHSE
processInput GHClib = processInputGHC

-- | Implementation of 'processInput' for the 'HSE' frontend.
processInputHSE
  :: Members '[GetOpt, Report] r
  => FilePath
  -> String
  -> Sem r (String, Maybe String)
processInputHSE inputFilename input =
  case HSE.parseModuleWithMode parseMode input of
    HSE.ParseOk inputModule -> do
      let intermediateModule = FromHSE.transformModule inputModule
      outputModule <- runEnv . runFresh $ do
        intermediateModule' <- processModule intermediateModule
        return $ ToHSE.transformModule inputModule intermediateModule'
      return
        (prettyPrintModuleHSE outputModule, getModuleName intermediateModule)
    HSE.ParseFailed srcLoc msg ->
      reportFatal
        $  Message Error
        $  msg
        ++ " in "
        ++ HSE.srcFilename srcLoc
        ++ ":"
        ++ show (HSE.srcLine srcLoc)
        ++ ":"
        ++ show (HSE.srcLine srcLoc)
        ++ "."
 where
   -- | Configuration of the @haskell-src-exts@ parser.
  parseMode :: HSE.ParseMode
  parseMode = HSE.defaultParseMode { HSE.parseFilename = inputFilename }

-- | Implementation of 'processInput' for the 'GHClib' frontend.
--
--   Reports all parsing errors and warnings that are reported by the @ghc-lib@
--   parser and cancels the computation if parsing fails and/or an error is
--   reported. The computation is canceled via the 'Cancel' effect directly
--   instead of 'Report'ing an additional fatal error message.
processInputGHC
  :: Members '[Cancel, GetOpt, Report] r
  => FilePath
  -> String
  -> Sem r (String, Maybe String)
processInputGHC inputFile input =
  case GHC.parseFile inputFile fakeDynFlags input of
    GHC.POk state locatedInputModule -> do
      reportParsingMessages state
      let inputModule        = GHC.unLoc locatedInputModule
          intermediateModule = FromGHC.transformModule inputModule
      outputModule <- runEnv . runFresh $ do
        intermediateModule' <- processModule intermediateModule
        return $ ToGHC.transformModule inputModule intermediateModule'
      return
        (prettyPrintModuleGHC outputModule, getModuleName intermediateModule)
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
  reportParsingMessages :: Members '[Cancel, Report] r => GHC.PState -> Sem r ()
  reportParsingMessages state = do
    let (warnings, errors) = GHC.getMessages state fakeDynFlags
    GHC.mapBagM_ (reportErrMsg Warning) warnings
    GHC.mapBagM_ (reportErrMsg Error) errors
    unless (GHC.isEmptyBag errors) cancel

  -- | Reports a error message or warning from @ghc-lib@.
  reportErrMsg :: Member Report r => Severity -> GHC.ErrMsg -> Sem r ()
  reportErrMsg severity msg =
    report
      $ Message severity
      $ intercalate "\n • "
      $ map (GHC.showSDoc fakeDynFlags)
      $ GHC.errDocImportant
      $ GHC.errMsgDoc msg

-- | Configuration of the @ghc-lib@ parser and pretty-printer.
fakeDynFlags :: GHC.DynFlags
fakeDynFlags = GHC.defaultDynFlags GHC.fakeSettings GHC.fakeLlvmConfig

-- | Gets the name of the given module.
getModuleName :: S.Module a -> Maybe String
getModuleName (S.Module moduleName _) = fmap getModuleName' moduleName
 where
  -- | Unwraps the given 'S.ModuleName'.
  getModuleName' :: S.ModuleName a -> String
  getModuleName' (S.ModuleName _ name) = name

-------------------------------------------------------------------------------
-- Output                                                                    --
-------------------------------------------------------------------------------

-- | Gets the name of the output file for a module that has been read from the
--   input file with the given name.
--
--   If the module has a module header, the output file name is based on the
--   module name. Otherwise, the base name of the input file is used.
makeOutputFileName
  :: FilePath     -- ^ The name of the input file.
  -> Maybe String -- ^ The name of the module to make the output file name of.
  -> FilePath     -- ^ The name of the output file.
makeOutputFileName inputFile modName = outputFileName <.> "hs"
 where
  -- | The output file name without file extension.
  outputFileName :: FilePath
  outputFileName =
    maybe (takeBaseName inputFile) (joinPath . splitOn ".") modName

-- | Pretty prints the given Haskell module with the pretty printer of
--   @haskell-src-exts@.
prettyPrintModuleHSE :: HSE.Module HSE.SrcSpanInfo -> String
prettyPrintModuleHSE = HSE.prettyPrintStyleMode
  (HSE.Style { HSE.mode           = HSE.PageMode
             , HSE.lineLength     = 120
             , HSE.ribbonsPerLine = 1.5
             }
  )
  HSE.defaultMode

-- | Pretty prints the given Haskell module with the pretty printer of
--   @ghc-lib@.
prettyPrintModuleGHC :: GHC.HsModule GHC.GhcPs -> String
prettyPrintModuleGHC = GHC.showPpr fakeDynFlags
