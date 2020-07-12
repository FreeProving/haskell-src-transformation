-- | This module contains the definition of and a parser for command line
--   options.

module HST.Options
  ( Options(..)
  , defaultOptions
    -- * Command Line Option Parser
  , optionDescriptors
  , parseArgs
  )
where

import           Polysemy                       ( Member
                                                , Sem
                                                )
import           System.Console.GetOpt          ( OptDescr(Option)
                                                , ArgDescr(NoArg, ReqArg)
                                                , ArgOrder(Permute)
                                                , getOpt
                                                )

import           HST.Effect.Report              ( Message(Message)
                                                , Report
                                                , Severity(Error)
                                                , report
                                                , reportFatal
                                                )

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

-------------------------------------------------------------------------------
-- Command Line Option Parser                                                --
-------------------------------------------------------------------------------

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
