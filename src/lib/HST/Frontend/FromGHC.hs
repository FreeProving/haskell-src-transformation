{-# LANGUAGE PackageImports, TypeFamilies #-}

module HST.Frontend.FromGHC where

import           Data.Maybe                     ( mapMaybe )

import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified "ghc-lib-parser" RdrName      as GHC
import qualified "ghc-lib-parser" OccName      as GHC

import qualified HST.Frontend.Syntax           as S

data GHC
type instance S.SrcSpanType GHC = GHC.SrcSpan
type instance S.Literal GHC = GHC.HsLit GHC.GhcPs
type instance S.TypeExp GHC = GHC.Located (GHC.HsType GHC.GhcPs)

transformModule :: GHC.HsModule GHC.GhcPs -> S.Module GHC
transformModule (GHC.HsModule { GHC.hsmodDecls = decls }) =
  S.Module (mapMaybe transformDecl decls)

transformDecl :: GHC.LHsDecl GHC.GhcPs -> Maybe (S.Decl GHC)
transformDecl (GHC.L _ (GHC.TyClD _ (dDecl@(GHC.DataDecl{})))) = Just
  (S.DataDecl (transformRdrNameUnqual (GHC.tcdLName dDecl))
              (transformDataDefn (GHC.tcdDataDefn dDecl))
  )
transformDecl _ = Nothing

transformDataDefn :: GHC.HsDataDefn GHC.GhcPs -> [S.ConDecl GHC]
transformDataDefn (GHC.HsDataDefn { GHC.dd_cons = cons }) =
  map transformConDecl cons
transformDataDefn _ = error "Unsupported data definition"

transformConDecl :: GHC.LConDecl GHC.GhcPs -> S.ConDecl GHC
transformConDecl (GHC.L _ conDecl@(GHC.ConDeclH98{})) = transformConDetails
  (GHC.con_args conDecl)
  (transformRdrNameUnqual (GHC.con_name conDecl))
transformConDecl _ = error "GADT constructors are not supported"

transformConDetails
  :: GHC.HsConDetails (GHC.LBangType GHC.GhcPs) recType
  -> S.Name GHC
  -> S.ConDecl GHC
transformConDetails (GHC.PrefixCon args) name = S.ConDecl name args
-- Maybe use a symbol instead of an ident name here (does that make a difference?)
transformConDetails (GHC.InfixCon arg1 arg2) name =
  S.InfixConDecl arg1 name arg2
transformConDetails _ _ = error "Record constructors are not supported"

transformRdrNameUnqual :: GHC.Located GHC.RdrName -> S.Name GHC
transformRdrNameUnqual (GHC.L s (GHC.Unqual occName)) =
  S.Ident (transformSrcSpan s) (GHC.occNameString occName)
transformRdrNameUnqual _ = error "Expected an unqualified name"

transformSrcSpan :: GHC.SrcSpan -> S.SrcSpan GHC
transformSrcSpan = S.SrcSpan
