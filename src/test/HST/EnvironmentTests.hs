-- | This module contains tests for "HST.Environment".
module HST.EnvironmentTests ( testEnvironment ) where

import           Polysemy                  ( Members, Sem )
import           Test.Hspec                ( Spec, describe, it, shouldBe )

import           HST.Application           ( createModuleInterface )
import           HST.Effect.Cancel         ( Cancel )
import           HST.Effect.Report         ( Report )
import           HST.Effect.SetExpectation ( SetExpectation, setExpectation )
import           HST.Effect.WithFrontend   ( WithFrontend )
import           HST.Environment           ( Environment(..), lookupTypeName )
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
--   declaration lists and the module interface for built-in data types.
setupTestEnvironment
  :: (Members '[Cancel, Report, SetExpectation, WithFrontend f] r)
  => [String]           -- ^ The lines of the current module.
  -> [[S.ImportDecl f]] -- ^ The import declaration lists.
  -> [[String]]         -- ^ The lines of the imported modules.
  -> Sem r (Environment f)
setupTestEnvironment currentModule importDecls importModules = do
  currentInterface <- createModuleInterface <$> parseTestModule currentModule
  importModules' <- mapM parseTestModule importModules
  let importInterfaces = map createModuleInterface importModules'
  return Environment { envCurrentModule   = currentInterface
                     , envImportedModules = zip importDecls importInterfaces
                     , envOtherEntries    = preludeModuleInterface
                     }

-- | Creates an import declaration based on the given values.
importDecl :: String -- ^ The name of the imported module.
           -> Bool   -- ^ @True@ if the import is qualified, @False@ otherwise.
           -> String -- ^ The alias name of the imported module. An empty
                     --   string signalizes an import without an alias name.
           -> S.ImportDecl a
importDecl modul isQual asMod = S.ImportDecl
  { S.importSrcSpan = S.NoSrcSpan
  , S.importModule  = moduleName modul
  , S.importIsQual  = isQual
  , S.importAsName  = if null asMod then Nothing else Just (moduleName asMod)
  }

-- | Creates a possibly qualified name based on the given values.
qName :: String -- ^ The module name the name is qualified by. An empty string
                --   signalizes an unqualified name.
      -> String -- ^ The name without its possible qualification. It is assumed
                --   that this is a regular identifier and not a symbol.
      -> S.QName a
qName "" name = S.UnQual S.NoSrcSpan (S.Ident S.NoSrcSpan name)
qName modul name =
  S.Qual S.NoSrcSpan (moduleName modul) (S.Ident S.NoSrcSpan name)

-- | Creates a module name based on the given name.
moduleName :: String -> S.ModuleName a
moduleName = S.ModuleName S.NoSrcSpan

-------------------------------------------------------------------------------
-- Test Modules                                                              --
-------------------------------------------------------------------------------
-- | Lines of the test module @modA@.
modA :: [String]
modA = [ "module A where"
       , "data Foo = Bar | Baz"
       ]

-------------------------------------------------------------------------------
-- Tests                                                                     --
-------------------------------------------------------------------------------
-- | Test group for "HST.Environment".
testEnvironment :: Spec
testEnvironment = describe "HST.Environment" $ do
  it "finds a type name in the current module" $
    runTest $ do
      env <- setupTestEnvironment modA [] []
      let query     = lookupTypeName (qName "" "Bar") env
          expResult = [(Just (moduleName "A"), Just (qName "" "Foo"))]
      setExpectation $ query `shouldBe` expResult
