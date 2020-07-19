{-# LANGUAGE PackageImports #-}

module HST.Frontend.ToGHC where

import qualified "ghc-lib-parser" GHC.Hs       as GHC
import qualified "ghc-lib-parser" SrcLoc       as GHC
import qualified "ghc-lib-parser" RdrName      as GHC
import qualified "ghc-lib-parser" TcEvidence   as GHC

import qualified HST.Frontend.Syntax           as S
import           HST.Frontend.FromGHC           ( GHC
                                                , LitWrapper(..)
                                                , TypeWrapper(..)
                                                )

transformModule
  :: GHC.HsModule GHC.GhcPs -> S.Module GHC -> GHC.HsModule GHC.GhcPs
transformModule oModule (S.Module aDecls) = oModule
  { GHC.hsmodDecls = combineDecls (GHC.hsmodDecls oModule)
                                  (map transformDecl (filter isFun aDecls))
  }
 where
  isFun (S.FunBind _ _) = True
  isFun _               = False
  combineDecls (GHC.L _ (GHC.ValD _ GHC.FunBind{}) : oDecls) (aDecl : aDecls')
    = aDecl : combineDecls oDecls aDecls'
  combineDecls (oDecl : oDecls) aDecls' = oDecl : combineDecls oDecls aDecls'
  combineDecls []               aDecls' = aDecls'

transformDecl :: S.Decl GHC -> GHC.LHsDecl GHC.GhcPs
transformDecl (S.DataDecl _ _) =
  error "Data type declarations should not be transformed back"
transformDecl (S.TypeSig s names (InOther typ)) = GHC.L
  (transformSrcSpan s)
  (GHC.SigD GHC.NoExtField
            (GHC.TypeSig GHC.NoExtField (map transformName names) typ)
  )
transformDecl (S.FunBind s matches) = GHC.L
  (transformSrcSpan s)
  (GHC.ValD
    GHC.NoExtField
    GHC.FunBind { GHC.fun_ext     = GHC.NoExtField
                , GHC.fun_id      = transformName (getMatchesName matches)
                , GHC.fun_matches = transformMatches matches
                , GHC.fun_co_fn   = GHC.WpHole
                , GHC.fun_tick    = []
                }
  )
 where
  getMatchesName :: [S.Match GHC] -> S.Name GHC
  getMatchesName (S.Match _ name _ _ _ : _) = name
  getMatchesName (S.InfixMatch _ _ name _ _ _ : _) = name
  getMatchesName _ = error "Empty match group"

transformName :: S.Name GHC -> GHC.Located GHC.RdrName
transformName = error ""

transformMatches
  :: [S.Match GHC] -> GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
transformMatches = error ""

transformSrcSpan :: S.SrcSpan GHC -> GHC.SrcSpan
transformSrcSpan (S.SrcSpan s) = s
transformSrcSpan S.NoSrcSpan   = GHC.noSrcSpan
