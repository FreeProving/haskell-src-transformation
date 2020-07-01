module HST.Frontend.Build where

import HST.Frontend.Syntax

name :: String -> Name ()
name = Ident ()

var :: Name () -> Exp () l t
var n = Var () (UnQual () n)

pvar :: Name () -> Pat () l
pvar = PVar ()

caseE :: Exp () l t -> [Alt () l t] -> Exp () l t
caseE = Case ()

alt :: Pat () l -> Exp () l t -> Alt () l t
alt pat e = Alt () pat (UnGuardedRhs () e) noBinds

binds :: [Decl () l t] -> Binds () l t
binds = BDecls ()

noBinds :: Maybe (Binds () l t)
noBinds = Nothing

wildcard :: Pat () l
wildcard = PWildCard ()
