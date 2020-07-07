-- | This module contains the command line interface for the
--   @haskell-src-transformations@ package.

module Main
  ( main
  )
where

import           Control.Exception              ( SomeException
                                                , displayException
                                                )
import           Control.Monad                  ( void )
import           Data.List.Extra                ( splitOn )
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
import           System.Console.GetOpt          ( OptDescr(Option)
                                                , ArgDescr(NoArg, ReqArg)
                                                , ArgOrder(Permute)
                                                , getOpt
                                                , usageInfo
                                                )
import           System.Environment             ( getProgName
                                                , getArgs
                                                )
import           System.Directory               ( createDirectoryIfMissing )
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                , joinPath
                                                , takeBaseName
                                                , takeDirectory
                                                )
import           System.IO                      ( stderr )

import           HST.Application                ( processModule
                                                , specialCons
                                                )
import           HST.Effect.Report              ( Message(Message)
                                                , Report
                                                , Severity
                                                  ( Internal
                                                  , Error
                                                  , Debug
                                                  )
                                                , msgSeverity
                                                , report
                                                , reportFatal
                                                , reportToHandleOrCancel
                                                , filterReportedMessages
                                                , exceptionToReport
                                                )
import           HST.Effect.Cancel              ( cancelToExit )
import           HST.Environment.FreshVars      ( PMState(PMState)
                                                , nextId
                                                , constrMap
                                                , matchedPat
                                                , trivialCC
                                                , opt
                                                , evalPM
                                                )

-------------------------------------------------------------------------------
-- Command Line Options                                                      --
-------------------------------------------------------------------------------

-- | A data type that contains the parsed command line options.
data Options = Options
  { optShowHelp     :: Bool
    -- ^ Flag that indicates whether to show the usage information.
  , optInputFiles   :: [FilePath]
    -- ^ The names of the input files.
  , optOutputDir    :: Maybe FilePath
    -- ^ The name of the output directory or @Nothing@ if output should be
    --   printed to the console.
  , optEnableDebug  :: Bool
    -- ^ Flag that indicates whether to print debugging messages to the console.
  , optTrivialCase  :: Bool
    -- ^ Flag that indicates whether to enable trivial case completion or not.
  , optOptimizeCase :: Bool
    -- ^ Flag that indicates whether optimization for case expressions is
    --   enabled or not.
  }

-- | The options to use by default if there are no command line arguments.
defaultOptions :: Options
defaultOptions = Options { optShowHelp     = False
                         , optInputFiles   = []
                         , optOutputDir    = Nothing
                         , optEnableDebug  = False
                         , optTrivialCase  = False
                         , optOptimizeCase = True
                         }

-- | Descriptors for the supported command line options.
--
--   The descriptors specify the name, alias and help message for the option
--   as well as a function that adds the flag or value to the 'Options'.
optionDescriptors :: [OptDescr (Options -> Options)]
optionDescriptors =
  [ Option ['h', '?']
           ["help"]
           (NoArg (\opts -> opts { optShowHelp = True }))
           "Display this message."
  , Option ['d']
           ["debug"]
           (NoArg (\opts -> opts { optEnableDebug = True }))
           "Enable printing of debugging messages."
  , Option ['t']
           ["trivial-cc"]
           (NoArg (\opts -> opts { optTrivialCase = True }))
           "Enable case completion with wildcard patterns."
  , Option ['n']
           ["no-optimization"]
           (NoArg (\opts -> opts { optOptimizeCase = False }))
           "Disable optimization for case expressions."
  , Option
    ['o']
    ["output"]
    (ReqArg (\dir opts -> opts { optOutputDir = Just dir }) "DIR")
    (  "Optional. Path to output directory.\n"
    ++ "Prints to the console by default."
    )
  ]

-- | Parses the given command line arguments.
--
--   Returns the recognized 'Options' and a list of non-options (i.e., input
--   file names).
parseArgs :: Member Report r => [String] -> Sem r Options
parseArgs args
  | null errors = do
    let opts = foldr ($) defaultOptions optSetters
    return opts { optInputFiles = nonOpts }
  | otherwise = do
    mapM_ (report . Message Error) errors
    reportFatal
      $  Message Error
      $  "Failed to parse command line arguments.\n"
      ++ "Use '--help' for usage information."
 where
  optSetters :: [Options -> Options]
  nonOpts :: [String]
  errors :: [String]
  (optSetters, nonOpts, errors) = getOpt Permute optionDescriptors args

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
application :: Members '[Report, Embed IO] r => Sem r ()
application = do
  args <- embed getArgs
  opts <- parseArgs args

  -- Filter reported message based on @--debug@ flag.
  filterReportedMessages
      (\msg -> optEnableDebug opts || msgSeverity msg /= Debug)
    $ -- Show usage information when the @--help@ flag is specified or there is no
      -- input file.
      if optShowHelp opts || null (optInputFiles opts)
        then embed putUsageInfo
        else mapM_ (processInputFile opts) (optInputFiles opts)

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
  :: Members '[Report, Embed IO] r => Options -> FilePath -> Sem r ()
processInputFile opts inputFile = do
  input <- embed $ readFile inputFile
  let state        = initPMState opts
      inputModule  = HSE.fromParseResult (HSE.parseModule input)
      outputModule = evalPM (processModule (void inputModule)) state
  case optOutputDir opts of
    Just outputDir -> do
      let outputFile = outputDir </> makeOutputFileName inputFile inputModule
      embed $ createDirectoryIfMissing True (takeDirectory outputFile)
      embed $ writeFile outputFile (prettyPrintModule outputModule)
    Nothing -> embed $ putStrLn (prettyPrintModule outputModule)

-- | Creates the initial 'PMState' from the given command line options.
initPMState :: Options -> PMState
initPMState opts = PMState { nextId     = 0
                           , constrMap  = specialCons
                           , matchedPat = []
                           , trivialCC  = optTrivialCase opts
                           , opt        = optOptimizeCase opts
                           }

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
prettyPrintModule :: HSE.Module () -> String
prettyPrintModule = HSE.prettyPrintStyleMode
  (HSE.Style { HSE.mode           = HSE.PageMode
             , HSE.lineLength     = 120
             , HSE.ribbonsPerLine = 1.5
             }
  )
  HSE.defaultMode
