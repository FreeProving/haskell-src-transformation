module HST.Frontend.Build where

import HST.Frontend.Syntax

name :: String -> Name s
name = Ident noSrc

var :: Name s -> Exp s l t
var n@(Ident s _) = Var s (UnQual s n)
var n@(Symbol s _) = Var s (UnQual s n)

pvar :: Name s -> Pat s l
pvar = PVar noSrc

caseE :: Exp s l t -> [Alt s l t] -> Exp s l t
caseE = Case noSrc

alt :: Pat s l -> Exp s l t -> Alt s l t
alt pat e = Alt (getSrcPat pat) pat (UnGuardedRhs (getSrcExp e) e) Nothing

binds :: [Decl s l t] -> Binds s l t
binds = BDecls noSrc

noBinds :: Maybe (Binds s l t)
noBinds = Nothing

wildcard :: Pat s l
wildcard = PWildCard noSrc

src :: s -> SrcSpan s
src = SrcSpan

noSrc :: SrcSpan s
noSrc = NoSrcSpan