-- | This module contains the command line interface for the
--   @haskell-src-transformations@ package.

module Main
  ( main
  )
where

import           Control.Exception              ( SomeException
                                                , displayException
                                                )
import           Data.List.Extra                ( splitOn )
import qualified Language.Haskell.Exts         as HSE
import           Polysemy                       ( Members
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
                                                , Severity(Internal, Debug)
                                                , msgSeverity
                                                , reportToHandleOrCancel
                                                , filterReportedMessages
                                                , exceptionToReport
                                                )
import           HST.Effect.Cancel              ( cancelToExit )
import           HST.Effect.Env                 ( runEnv )
import           HST.Effect.Fresh               ( runFresh )
import           HST.Effect.GetOpt              ( GetOpt
                                                , getOpt
                                                , runWithArgsIO
                                                )
import qualified HST.Frontend.FromHSE          as FromHSE
import qualified HST.Frontend.ToHSE            as ToHSE
import           HST.Options                    ( optShowHelp
                                                , optInputFiles
                                                , optOutputDir
                                                , optEnableDebug
                                                , optionDescriptors
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
application :: Members '[Embed IO, GetOpt, Report] r => Sem r ()
application = do
  -- Filter reported message based on @--debug@ flag.
  debuggingEnabled <- getOpt optEnableDebug
  filterReportedMessages (\msg -> debuggingEnabled || msgSeverity msg /= Debug)
    $ do
        -- Show usage information when the @--help@ flag is specified or there is no
        -- input file.
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
  :: Members '[Embed IO, GetOpt, Report] r => FilePath -> Sem r ()
processInputFile inputFile = do
  input <- embed $ readFile inputFile
  let inputModule = HSE.fromParseResult (HSE.parseModule input)
  intermediateModule <- FromHSE.transformModule inputModule
  outputModule       <- runEnv . runFresh $ do
    intermediateModule' <- processModule intermediateModule
    return $ ToHSE.transformModule inputModule intermediateModule'
  maybeOutputDir <- getOpt optOutputDir
  case maybeOutputDir of
    Just outputDir -> do
      let outputFile = outputDir </> makeOutputFileName inputFile inputModule
      embed $ createDirectoryIfMissing True (takeDirectory outputFile)
      embed $ writeFile outputFile (prettyPrintModule outputModule)
    Nothing -> embed $ putStrLn (prettyPrintModule outputModule)

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
  -> HSE.Module l -- ^ The module to make the output file name of.
  -> FilePath     -- ^ The name of the output file.
makeOutputFileName inputFile inputModule = outputFileName <.> "hs"
 where
  -- | The output file name without file extension.
  outputFileName :: FilePath
  outputFileName = maybe (takeBaseName inputFile)
                         (joinPath . splitOn ".")
                         (moduleName inputModule)

  -- | Gets the module name of the given module.
  --
  --   Returns @Nothing@ if there is no module header or the module type is not
  --   supported.
  moduleName :: HSE.Module l -> Maybe String
  moduleName (HSE.Module _ (Just moduleHead) _ _ _) = case moduleHead of
    (HSE.ModuleHead _ (HSE.ModuleName _ modName) _ _) -> Just modName
  moduleName _ = Nothing

-- | Pretty prints the given Haskell module.
prettyPrintModule :: HSE.Module HSE.SrcSpanInfo -> String
prettyPrintModule = HSE.prettyPrintStyleMode
  (HSE.Style { HSE.mode           = HSE.PageMode
             , HSE.lineLength     = 120
             , HSE.ribbonsPerLine = 1.5
             }
  )
  HSE.defaultMode
