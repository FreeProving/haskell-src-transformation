-- | This module contains functions for pretty printing names of the
--   intermediate representation for error messages.

module HST.Util.PrettyName
  ( PrettyName(prettyName)
  )
where

import qualified HST.Frontend.Syntax           as S

-- | Type class for name AST nodes that can be pretty printed.
class PrettyName a where
  prettyName :: a -> String

-- | Pretty prints a module name.
instance PrettyName (S.ModuleName a) where
  prettyName (S.ModuleName _ modName) = modName

-- | Pretty prints a possibly qualified name.
instance PrettyName (S.QName a) where
  prettyName (S.Qual _ modName name) =
    prettyNameWithPrefix (prettyName modName ++ ".") name
  prettyName (S.UnQual  _ name      ) = prettyName name
  prettyName (S.Special _ specialCon) = prettyName specialCon

-- | Pretty prints a name.
instance PrettyName (S.Name a) where
  prettyName = prettyNameWithPrefix ""

-- | Helper functions for 'PrettyName' instance of 'S.Name' and 'S.QName'
--   that pretty prints a name but adds a prefix.
--
--    The prefix is added before the identifier or symbol. In case of symbolic
--    names, the prefix is within the parentheses.
prettyNameWithPrefix :: String -> S.Name a -> String
prettyNameWithPrefix prefix (S.Ident  _ ident) = prefix ++ ident
prettyNameWithPrefix prefix (S.Symbol _ sym  ) = "(" ++ prefix ++ sym ++ ")"

-- | Pretty prints a possibly qualified infix operator.
instance PrettyName (S.QOp a) where
  prettyName (S.QVarOp _ name) = prettyName name
  prettyName (S.QConOp _ name) = prettyName name

-- | Pretty prints the name of a special constructor.
instance PrettyName (S.SpecialCon a) where
  prettyName (S.UnitCon _) = "()"
  prettyName (S.ListCon _) = "[]"
  prettyName (S.FunCon  _) = "(->)"
  prettyName (S.TupleCon _ boxed n) =
    "(" ++ prettyBoxed ++ replicate n ',' ++ prettyBoxed ++ ")"
   where
    prettyBoxed = case boxed of
      S.Boxed   -> ""
      S.Unboxed -> "#"
  prettyName (S.Cons             _) = "(:)"
  prettyName (S.UnboxedSingleCon _) = "(# #)"
  prettyName (S.ExprHole         _) = "_"
