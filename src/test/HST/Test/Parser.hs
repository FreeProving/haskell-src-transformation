-- | This module contains functions for parsing AST nodes for testing purposes.
module HST.Test.Parser where

import           Control.Monad                              ( (>=>) )
import qualified Language.Haskell.Exts                      as HSE
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as GHC
import           Polysemy
  ( Member, Members, Sem )

import           HST.Effect.Cancel                          ( Cancel )
import           HST.Effect.Report                          ( Report )
import           HST.Effect.WithFrontend
  ( WithFrontend, parseModule, transformModule )
import           HST.Frontend.GHC.Config
  ( GHC, defaultDynFlags )
import qualified HST.Frontend.GHC.From                      as FromGHC
import           HST.Frontend.HSE.Config                    ( HSE )
import qualified HST.Frontend.HSE.From                      as FromHSE
import           HST.Frontend.Parser
  ( handleParseResultGHC, handleParseResultHSE )
import qualified HST.Frontend.Syntax                        as S

-- | Parses a module for testing purposes.
parseTestModule :: Members '[Cancel, Report, WithFrontend f] r
                => [String]
                -> Sem r (S.Module f)
parseTestModule = (parseModule "<test-input>" >=> transformModule) . unlines

-- | Parses an expression with the GHC front end for testing purposes.
parseTestExpressionGHC
  :: Members '[Cancel, Report] r => [String] -> Sem r (S.Exp GHC)
parseTestExpressionGHC expString = handleParseResultGHC
  (GHC.parseExpression (unlines expString) defaultDynFlags)
  >>= FromGHC.transformExpr

-- | Parses an expression with the HSE front end for testing purposes.
parseTestExpressionHSE :: Member Report r => [String] -> Sem r (S.Exp HSE)
parseTestExpressionHSE expString = handleParseResultHSE
  (HSE.parseExp (unlines expString))
  >>= FromHSE.transformExp
