{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the command line interface for the
--   @haskell-src-transformations@ package.
module Main ( main ) where

import           Control.Exception       ( SomeException, displayException )
import           Data.List.Extra         ( splitOn )
import           Polysemy                ( Members, Sem )
import           Polysemy.Embed          ( Embed, embed )
import           Polysemy.Final          ( embedToFinal, runFinal )
import           System.Console.GetOpt   ( usageInfo )
import           System.Directory        ( createDirectoryIfMissing )
import           System.Environment      ( getProgName )
import           System.FilePath
  ( (<.>), (</>), joinPath, takeBaseName, takeDirectory )
import           System.IO               ( stderr )

import           HST.Application
  ( createModuleInterface, initializeEnvironment, processModule )
import           HST.Effect.Cancel       ( Cancel, cancelToExit )
import           HST.Effect.Env          ( runWithEnv )
import           HST.Effect.Fresh        ( runFresh )
import           HST.Effect.GetOpt       ( GetOpt, getOpt, runWithArgsIO )
import           HST.Effect.InputFile
  ( InputFile, getInputFile, runInputFile )
import           HST.Effect.InputModule
  ( InputModule, ModuleInterface, getInputModule, runInputModule )
import           HST.Effect.Report
  ( Report, exceptionToReport, filterReportedMessages, reportToHandleOrCancel )
import           HST.Effect.WithFrontend
  ( WithFrontend, parseModule, prettyPrintModule, runWithFrontend )
import qualified HST.Frontend.Syntax     as S
import           HST.Options
  ( optEnableDebug, optFrontend, optInputFiles, optOutputDir, optShowHelp
  , optionDescriptors, parseFrontend )
import           HST.Util.Messages
  ( Message, Severity(Debug, Internal), message, msgSeverity )
import           HST.Util.PrettyName     ( prettyName )
import           HST.Util.Selectors      ( findIdentifiers )

-------------------------------------------------------------------------------
-- Usage Information                                                         --
-------------------------------------------------------------------------------
-- | The header of the help message.
--
--   This text is added before the description of the command line arguments.
usageHeader :: FilePath -> String
usageHeader progName = "Usage: "
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
main = runFinal
  . embedToFinal
  . cancelToExit
  . runInputFile
  . reportToHandleOrCancel stderr
  . exceptionToReport exceptionToMessage
  . runWithArgsIO
  $ application
 where
  exceptionToMessage :: SomeException -> Message
  exceptionToMessage e = message Internal S.NoSrcSpan (displayException e)

-- | The main computation of the command line interface.
--
--   Parses the command line arguments and input file. The transformation is
--   applied on the parsed input module and a state constructed from the
--   command line arguments. The output is either printed to the console
--   or a file.
application
  :: Members '[Cancel, Embed IO, GetOpt, InputFile, Report] r => Sem r ()
application = do
  -- Filter reported message based on @--debug@ flag.
  debuggingEnabled <- getOpt optEnableDebug
  filterReportedMessages (\msg -> debuggingEnabled || msgSeverity msg /= Debug)
    $ do
      -- Show usage information when the @--help@ flag is specified or there
      -- is no input file.
      showHelp <- getOpt optShowHelp
      inputFiles <- getOpt optInputFiles
      if showHelp || null inputFiles then embed putUsageInfo else do
        frontend <- parseFrontend =<< getOpt optFrontend
        runWithFrontend frontend $ do
          mods <- mapM performTransformation inputFiles
          runInputModule (zip inputFiles mods)
            $ mapM_ processInputModule inputFiles

-------------------------------------------------------------------------------
-- Pattern Matching Compilation                                              --
-------------------------------------------------------------------------------
-- | Reads, parses and transforms the module at the given file path and returns
--   the module transformed to the HST syntax and the created module interface.
performTransformation
  :: Members '[Cancel, Embed IO, InputFile, Report, WithFrontend f] r
  => FilePath
  -> Sem r (S.Module f, ModuleInterface f)
performTransformation inputFilename = do
  input <- getInputFile inputFilename
  inputModule <- parseModule inputFilename input
  return (inputModule, createModuleInterface inputModule)

-- | Initializes the environment for the module at the given file path, applies
--   the pattern matching compilation to it and writes a pretty printed version
--   of the processed module to the console or an output file depending on the
--   command line options.
--
--   The name of the output file is generated by appending the result of
--   'makeOutputFileName' for the input module to the output directory.
--   If the output directory does not exist, the output directory and all of
--   its parent directories are created.
processInputModule
  :: forall f r.
  ( Members '[Cancel, Embed IO, GetOpt, InputModule f, Report, WithFrontend f] r
  , S.EqAST f
  )
  => FilePath
  -> Sem r ()
processInputModule inputFilename = do
  inputModule <- getInputModule inputFilename
  env <- initializeEnvironment inputFilename
  outputModule <- runWithEnv env . runFresh (findIdentifiers inputModule)
    $ processModule inputModule
  output <- prettyPrintModule outputModule
  let moduleName = getModuleName inputModule
  maybeOutputDir <- getOpt optOutputDir
  case maybeOutputDir of
    Just outputDir -> do
      let outputFilename
            = outputDir </> makeOutputFileName inputFilename moduleName
      embed $ createDirectoryIfMissing True (takeDirectory outputFilename)
      embed $ writeFile outputFilename output
    Nothing        -> embed $ putStrLn output
 where
  -- | Gets the name of the given module.
  getModuleName :: S.Module a -> Maybe String
  getModuleName (S.Module _ _ moduleName _ _) = fmap prettyName moduleName

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
  outputFileName = maybe (takeBaseName inputFile) (joinPath . splitOn ".")
    modName
