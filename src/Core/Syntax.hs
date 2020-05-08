{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Syntax where


import           Type                           ( Name(..)
                                                , Type
                                                )
import           ClangType
import           Language.C.Clang.Cursor        ( cursorKind
                                                , cursorSpelling
                                                , Cursor
                                                )
import           Control.Lens
import           Data.Functor                   ( ($>) )
import           Clang
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

-- | All available expressions
-- Warning: The cursors are NOT injective! 
-- Multiple different expressions can have and indeed WILL have the same cursor.
data Expr' t c = Var c Name
               | App c (Expr' t c) (Expr' t c)
               | Lam c t Name (Expr' t c)
               | LetIn c Name (Expr' t c) (Expr' t c)
               | Literal c
               | If c (Expr' t c) (Expr' t c) (Expr' t c)
               | Fix c (Expr' t c)
               | Builtin c BuiltinExpr
          deriving stock (Eq, Ord, Show, Functor)

--          | Ctor Name Expr
--          | Elim Name [Name] Expr Expr

data BuiltinExpr = BuiltinBinOp BinOp
                 | BuiltinUnOp UnOp
                 | BuiltinMemberRef
                 | BuiltinNew Type
                 | BuiltinArraySubscript
                 | BuiltinUnit
                 | BuiltinNullPtr
  deriving stock (Eq, Show, Ord)

instance Pretty BuiltinExpr where
  pretty (BuiltinBinOp bop) =
    "#builtin_binop_" <> pretty (drop (length ("BinOp" :: String)) (show bop))
  pretty (BuiltinUnOp uop) =
    "#builtin_unop_" <> pretty (drop (length ("UnOp" :: String)) (show uop))
  pretty BuiltinMemberRef      = "#builtin_memberref"
  pretty (BuiltinNew typ) = "#builtin_new" <+> "@" <> PP.parens (pretty typ)
  pretty BuiltinArraySubscript = "#builtin_arrsubscript"
  pretty BuiltinUnit           = "#builtin_unit"
  pretty BuiltinNullPtr        = "#builtin_nullptr"

type Expr = Expr' (Maybe ClangType) Cursor
type CursorExpr t = Expr' t Cursor

data Let t c = Let c Name (Expr' t c)
  deriving stock (Eq, Ord, Show, Functor)

instance Pretty (Let t Cursor) where
  pretty (Let _ name expr) =
    "let" <+> pretty name <+> "=" <+> PP.hang 4 (pretty expr)

data TopLevel' t c = TLLet (Let t c)
                   | TLLetRecursive [Let t c]
                   | TLLetNoBody c Name
  deriving stock (Eq, Ord, Show, Functor)
makePrisms ''TopLevel'

type TopLevel = TopLevel' (Maybe ClangType) Cursor

instance Pretty (TopLevel' t Cursor) where
  pretty (TLLet          l ) = pretty l
  pretty (TLLetRecursive ls) = "rec" <+> PP.align (PP.vsep $ pretty <$> ls)
  pretty (TLLetNoBody _ name) =
    PP.enclose "(*" "*)" $ "predeclared:" <+> "let" <+> pretty name

instance Pretty (Expr' t Cursor) where
  pretty (Var _ name       ) = pretty name
  pretty (App _ e1 e2@App{}) = pretty e1 <+> PP.parens (pretty e2)
  pretty (App _ e1 e2      ) = pretty e1 <+> pretty e2
  pretty (Lam _ _ name expr) = "\\" <> pretty name <+> "->" <+> pretty expr
  pretty (LetIn _ name e1 e2) =
    "let" <+> pretty name <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
  pretty (If _ cond thn els) = "if" <+> PP.align
    (PP.vsep [pretty cond, "then" <+> pretty thn, "else" <+> pretty els])
  pretty (Literal c) =
    let spelling = BS.unpack $ cursorSpelling c
    in  if null spelling
          then PP.angles (pretty $ show $ cursorKind c)
          else pretty spelling
  pretty (Builtin _ b) = pretty b

class HasCursor a where
  cursorL :: Lens' a Cursor

instance HasCursor (CursorExpr t) where
  cursorL = lens getter setter
   where
    getter :: CursorExpr t -> Cursor
    getter = \case
      Var c _       -> c
      App c _ _     -> c
      Lam   c _ _ _ -> c
      LetIn c _ _ _ -> c
      Literal c     -> c
      If c _ _ _    -> c
      Fix c _       -> c
    setter :: CursorExpr t -> Cursor -> CursorExpr t
    setter c newCursor = c $> newCursor

-- this instance is a bit moot, but...
instance HasCursor Cursor where
  cursorL = lens id const

instance HasCursor (Let t Cursor) where
  cursorL = lens getter setter
   where
    getter :: Let t Cursor -> Cursor
    getter (Let c _ _) = c
    setter :: Let t Cursor -> Cursor -> Let t Cursor
    setter l newCursor = l $> newCursor

instance HasCursor TopLevel where
  cursorL = lens getter setter
   where
    getter :: TopLevel -> Cursor
    getter = \case
      TLLet          l       -> view cursorL l
      TLLetRecursive (l : _) -> view cursorL l
      TLLetNoBody c _        -> c
    setter :: TopLevel -> Cursor -> TopLevel
    setter c newCursor = c $> newCursor

unit :: Cursor -> Expr
unit c = Builtin c BuiltinUnit
