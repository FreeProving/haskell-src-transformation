module HST.Frontend.Build where

import HST.Frontend.Syntax

var :: Name s -> Exp s l t
var n@(Ident s _) = Var s (UnQual s n)
var n@(Symbol s _) = Var s (UnQual s n)

alt :: Pat s l -> Exp s l t -> Alt s l t
alt pat e = Alt (getSrcPat pat) pat (UnGuardedRhs (getSrcExp e) e) Nothing