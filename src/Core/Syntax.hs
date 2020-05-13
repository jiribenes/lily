{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Syntax where


import           Control.Lens
import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )
import           Language.C.Clang.Cursor        ( Cursor
                                                , cursorKind
                                                , cursorSpelling
                                                )

import           Clang.OpParser
import           Name
import           Type.Type                      ( Type )

-- | All available expressions
-- Warning: The cursors are NOT injective! 
-- Multiple different expressions can have and indeed WILL have the same cursor.
data Expr' t c = Var c Name
               | App c (Expr' t c) (Expr' t c)
               | Lam c Name (Expr' t c)
               | LetIn c Name (Expr' t c) (Expr' t c)
               | Literal c t
               | If c (Expr' t c) (Expr' t c) (Expr' t c)
               | Builtin c BuiltinExpr
          deriving stock (Eq, Ord, Show, Functor)

--          | Ctor Name Expr
--          | Elim Name [Name] Expr Expr

data BuiltinExpr = BuiltinBinOp BinOp Type Type
                 | BuiltinUnOp UnOp Type Type
                 | BuiltinMemberRef
                 | BuiltinNew Type
                 | BuiltinNewArray Type
                 | BuiltinArraySubscript Type Type
                 | BuiltinUnit
                 | BuiltinNullPtr
  deriving stock (Eq, Show, Ord)

instance Pretty BuiltinExpr where
  pretty (BuiltinBinOp bop resultType opType) =
    "#builtin_binop_"
      <>  pretty (drop (length ("BinOp" :: String)) (show bop))
      <+> "@"
      <>  PP.parens (pretty resultType)
      <+> "@"
      <>  PP.parens (pretty opType)
  pretty (BuiltinUnOp uop resultType opType) =
    "#builtin_unop_"
      <>  pretty (drop (length ("UnOp" :: String)) (show uop))
      <+> "@"
      <>  PP.parens (pretty resultType)
      <+> "@"
      <>  PP.parens (pretty opType)
  pretty BuiltinMemberRef = "#builtin_memberref"
  pretty (BuiltinNew typ) = "#builtin_new" <+> "@" <> PP.parens (pretty typ)
  pretty (BuiltinNewArray typ) =
    "#builtin_new_array" <+> "@" <> PP.parens (pretty typ)
  pretty (BuiltinArraySubscript arrayTyp subscriptTyp) =
    "#builtin_arrsubscript"
      <+> "@"
      <>  PP.parens (pretty arrayTyp)
      <+> "@"
      <>  PP.parens (pretty subscriptTyp)
  pretty BuiltinUnit    = "#builtin_unit"
  pretty BuiltinNullPtr = "#builtin_nullptr"

type Expr = Expr' (Maybe Type) Cursor
type CursorExpr t = Expr' t Cursor

data Let' t c = Let c Name (Expr' t c)
  deriving stock (Eq, Ord, Show, Functor)

type Let = Let' (Maybe Type) Cursor

instance Pretty t => Pretty (Let' t Cursor) where
  pretty (Let _ name expr) =
    "let" <+> pretty name <+> "=" <+> PP.hang 4 (pretty expr)

data StructField' t c = StructField c t Name
  deriving stock (Eq, Ord, Show, Functor)

instance Pretty t => Pretty (StructField' t Cursor) where
  pretty (StructField _ t n) = pretty n <+> "::" <+> pretty t

type StructField = StructField' (Maybe Type) Cursor

data Struct' t c = Struct c Name [StructField' t c]
  deriving stock (Eq, Ord, Show, Functor)

type Struct = Struct' (Maybe Type) Cursor

instance Pretty t => Pretty (Struct' t Cursor) where
  pretty (Struct _ name s) = "struct" <+> pretty name <+> "=" <+> PP.align
    (PP.encloseSep PP.lbrace PP.rbrace PP.comma $ pretty <$> s)

data TopLevel' t c = TLLet (Let' t c)
                   | TLLetRecursive (NonEmpty (Let' t c))
                   | TLLetNoBody c Name
                   | TLStruct (Struct' t c)
  deriving stock (Eq, Ord, Show, Functor)
makePrisms ''TopLevel'

type TopLevel = TopLevel' (Maybe Type) Cursor

instance Pretty t => Pretty (TopLevel' t Cursor) where
  pretty (TLLet l) = pretty l
  pretty (TLLetRecursive ls) =
    "rec" <+> PP.align (PP.vsep $ NE.toList $ pretty <$> ls)
  pretty (TLLetNoBody _ name) =
    PP.enclose "(*" "*)" $ "predeclared:" <+> "let" <+> pretty name
  pretty (TLStruct s) = pretty s

instance Pretty t => Pretty (Expr' t Cursor) where
  pretty (Var _ name         ) = pretty name
  pretty (App _ e1   e2@App{}) = pretty e1 <+> PP.parens (pretty e2)
  pretty (App _ e1   e2      ) = pretty e1 <+> pretty e2
  pretty (Lam _ name expr    ) = "\\" <> pretty name <+> "->" <+> pretty expr
  pretty (LetIn _ name e1 e2) =
    PP.align (PP.sep ["let" <+> pretty name <+> "=" <+> pretty e1, "in" <+> pretty e2])
  pretty (If _ cond thn els) = "if" <+> PP.align
    (PP.vsep [pretty cond, "then" <+> pretty thn, "else" <+> pretty els])
  pretty (Literal c t) =
    let spelling = BS.unpack $ cursorSpelling c
    in  (if null spelling
          then PP.angles (pretty $ show $ cursorKind c)
          else pretty spelling
        )
          <+> "@"
          <>  PP.parens (pretty t)
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
      Lam c _ _     -> c
      LetIn c _ _ _ -> c
      Literal c _   -> c
      If c _ _ _    -> c
      Builtin c _   -> c
    setter :: CursorExpr t -> Cursor -> CursorExpr t
    setter c newCursor = c $> newCursor

-- this instance is a bit moot, but...
instance HasCursor Cursor where
  cursorL = lens id const

instance HasCursor (Let' t Cursor) where
  cursorL = lens getter setter
   where
    getter :: Let' t Cursor -> Cursor
    getter (Let c _ _) = c
    setter :: Let' t Cursor -> Cursor -> Let' t Cursor
    setter l newCursor = l $> newCursor

instance HasCursor TopLevel where
  cursorL = lens getter setter
   where
    getter :: TopLevel -> Cursor
    getter = \case
      TLLet          l        -> view cursorL l
      TLLetRecursive (l :| _) -> view cursorL l
      TLLetNoBody c _         -> c
      TLStruct s              -> view cursorL s
    setter :: TopLevel -> Cursor -> TopLevel
    setter c newCursor = c $> newCursor

instance HasCursor (Struct' t Cursor) where
  cursorL = lens (\(Struct c _ _) -> c) ($>)

unit :: Cursor -> Expr
unit c = Builtin c BuiltinUnit

class HasName a where
  nameL :: Lens' a Name

instance HasName (Let' t c) where
  nameL = lens getter setter
   where
    getter :: Let' t c -> Name
    getter (Let _ n _) = n
    setter (Let c _ e) n = Let c n e
