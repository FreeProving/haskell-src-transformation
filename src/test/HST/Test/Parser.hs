-- | This module contains functions for parsing AST nodes for testing purposes.
module HST.Test.Parser where

import           Control.Monad           ( (>=>) )
import           Polysemy                ( Members, Sem )

import           HST.Effect.Cancel       ( Cancel )
import           HST.Effect.Report       ( Report )
import           HST.Effect.WithFrontend
  ( WithFrontend, parseExp, parseModule, transformExp, transformModule )
import qualified HST.Frontend.Syntax     as S

-- | Parses a module for testing purposes.
parseTestModule :: Members '[Cancel, Report, WithFrontend f] r
                => [String]
                -> Sem r (S.Module f)
parseTestModule = (parseModule "<test-input>" >=> transformModule) . unlines

-- | Parses an expression for testing purposes.
parseTestExp
  :: Members '[Cancel, Report, WithFrontend f] r => String -> Sem r (S.Exp f)
parseTestExp = parseExp >=> transformExp
