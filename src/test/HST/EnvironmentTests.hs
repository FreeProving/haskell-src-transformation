-- | This module contains tests for "HST.Environment".
module HST.EnvironmentTests ( testEnvironment ) where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, describe, it, shouldBe )

import           HST.Application           ( createModuleInterface )
import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation, setExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import           HST.Environment           ( Environment(..) )
import           HST.Environment.Prelude   ( preludeModuleInterface )
import qualified HST.Frontend.Syntax       as S
import           HST.Test.Parser           ( parseTestModule )
import           HST.Test.Runner           ( runTest )

-------------------------------------------------------------------------------
-- Utility Functions                                                         --
-------------------------------------------------------------------------------
-- | Parses the given modules, creates module interfaces for them and sets up
--   an environment containing the the interface of the current module, the
--   interfaces of the imported modules combined with the given import
--   declarations and the module interface for built-in data types.
setupTestEnvironment
  :: (Members '[Cancel, Report, SetExpectation, WithFrontend f] r)
  => [String]         -- ^ The lines of the current module.
  -> [S.ImportDecl f] -- ^ The import declarations.
  -> [[String]]       -- ^ The lines of the imported modules.
  -> Sem r (Environment f)
setupTestEnvironment currentModule importDecls importModules = do
  currentInterface <- createModuleInterface <$> parseTestModule currentModule
  importModules' <- mapM parseTestModule importModules
  let importInterfaces = map createModuleInterface importModules'
  return Environment { envCurrentModule   = currentInterface
                     , envImportedModules = zip importDecls importInterfaces
                     , envOtherEntries    = preludeModuleInterface
                     }

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
-- | Test group for "HST.Environment".
testEnvironment :: Spec
testEnvironment = describe "HST.Environment" $ do
  it "" $
    runTest $ do
      _ <- setupTestEnvironment [] [] []
      setExpectation (() `shouldBe` ())
