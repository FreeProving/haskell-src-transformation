-- | This module contains the definition of and a parser for command line
--   options.
module HST.Options
  ( Options(..)
  , defaultOptions
    -- Frontend Options
  , Frontend(..)
  , frontendMap
  , hseFrontendName
  , ghclibFrontendName
    -- * Command Line Option Parser
  , optionDescriptors
  , parseArgs
  , parseFrontend
  ) where

import           Data.List             ( intercalate )
import           Data.Map.Strict       ( Map )
import qualified Data.Map.Strict       as Map
import           Polysemy              ( Member, Sem )
import           System.Console.GetOpt
  ( ArgDescr(NoArg, ReqArg), ArgOrder(Permute), OptDescr(Option), getOpt )

import           HST.Effect.Report     ( Report, report, reportFatal )
import           HST.Util.Messages
  ( Severity(Error), messageWithoutSrcSpan )

-- | A data type for all front ends that can be used for parsing the given input
--   program in @haskell-src-transformations@.
data Frontend = HSE | GHClib
 deriving ( Eq, Show )

-- | Name of the 'HSE' front end.
hseFrontendName :: String
hseFrontendName = "haskell-src-exts"

-- | Name of the 'GHClib' front end.
ghclibFrontendName :: String
ghclibFrontendName = "ghc-lib"

-- | Map that maps strings to the corresponding front ends. Used for parsing
--   the front end option.
frontendMap :: Map String Frontend
frontendMap
  = Map.fromList [(hseFrontendName, HSE), (ghclibFrontendName, GHClib)]

-- | Parses a given string to one of the front ends.
parseFrontend :: Member Report r => String -> Sem r Frontend
parseFrontend s = case Map.lookup s frontendMap of
  Nothing -> reportFatal
    $ messageWithoutSrcSpan Error
    $ "Unavailable front end.\n" ++ "Use '--help' for allowed values."
  Just f  -> return f

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
  , optFrontend     :: String
    -- ^ The front end used for parsing the input program.
  }

-- | The options to use by default if there are no command line arguments.
defaultOptions :: Options
defaultOptions = Options
  { optShowHelp     = False
  , optInputFiles   = []
  , optOutputDir    = Nothing
  , optEnableDebug  = False
  , optTrivialCase  = False
  , optOptimizeCase = True
  , optFrontend     = hseFrontendName
  }

-------------------------------------------------------------------------------
-- Command Line Option Parser                                                --
-------------------------------------------------------------------------------
-- | Descriptors for the supported command line options.
--
--   The descriptors specify the name, alias and help message for the option
--   as well as a function that adds the flag or value to the 'Options'.
optionDescriptors :: [OptDescr (Options -> Options)]
optionDescriptors
  = [ Option ['h', '?'] ["help"] (NoArg (\opts -> opts { optShowHelp = True }))
        "Display this message."
    , Option ['d'] ["debug"] (NoArg (\opts -> opts { optEnableDebug = True }))
        "Enable printing of debugging messages."
    , Option ['t'] ["trivial-cc"]
        (NoArg (\opts -> opts { optTrivialCase = True }))
        "Enable case completion with wildcard patterns."
    , Option ['n'] ["no-optimization"]
        (NoArg (\opts -> opts { optOptimizeCase = False }))
        "Disable optimization for case expressions."
    , Option ['o'] ["output"]
        (ReqArg (\dir opts -> opts { optOutputDir = Just dir }) "DIR")
        ("Optional. Path to output directory.\n"
         ++ "Prints to the console by default.")
    , Option ['f'] ["frontend"]
        (ReqArg (\f opts -> opts { optFrontend = f }) "FRONTEND")
        ("Optional. Specifies the front end for the compiler to use.\n"
         ++ "Allowed values are: "
         ++ intercalate ", " (map (\s -> '`' : s ++ "`") (Map.keys frontendMap))
         ++ ".\n"
         ++ "Uses `"
         ++ optFrontend defaultOptions
         ++ "` by default.")
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
    mapM_ (report . messageWithoutSrcSpan Error) errors
    reportFatal
      $ messageWithoutSrcSpan Error
      $ "Failed to parse command line arguments.\n"
      ++ "Use '--help' for usage information."
 where
  optSetters :: [Options -> Options]
  nonOpts :: [String]
  errors :: [String]
  (optSetters, nonOpts, errors) = getOpt Permute optionDescriptors args
