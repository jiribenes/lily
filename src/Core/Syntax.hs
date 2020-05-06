{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Syntax where


import           Type                           ( Name(..) )
import           ClangType
import           Language.C.Clang.Cursor        ( cursorKind
                                                , cursorSpelling
                                                , Cursor
                                                )
import           Control.Lens
import           Data.Functor                   ( ($>) )
import           Clang
import qualified Data.ByteString.Char8         as BS

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

--          | Ctor Name Expr
--          | Elim Name [Name] Expr Expr
          deriving (Eq, Ord)

data BuiltinExpr = BuiltinBinOp BinOp
                 | BuiltinUnOp UnOp
                 | BuiltinUnit
  deriving (Eq, Ord)

instance Show BuiltinExpr where
  show (BuiltinBinOp bop) =
    "@builtin_binop_" <> drop (length "BinOp") (show bop)
  show (BuiltinUnOp uop) = "@builtin_unop_" <> drop (length "UnOp") (show uop)
  show BuiltinUnit       = "@builtin_unit"

deriving instance (Functor (Expr' t))

type Expr = Expr' (Maybe ClangType) Cursor
type CursorExpr t = Expr' t Cursor

data Let t c = Let c Name (Expr' t c)
  deriving (Eq, Ord)

deriving instance (Functor (Let t))

instance Show (Let t Cursor) where
  show (Let _ name expr) = "let " <> show name <> " = " <> show expr

data TopLevel' t c = TLLet (Let t c)
                   | TLLetRecursive [Let t c]
                   | TLLetNoBody c Name
  deriving (Eq, Ord)

makePrisms ''TopLevel'

deriving instance (Functor (TopLevel' t))

type TopLevel = TopLevel' (Maybe ClangType) Cursor

instance Show TopLevel where
  show (TLLet l) = show l
  show (TLLetRecursive lets) =
    "recgroup\n" <> unlines (("\t" <>) . show <$> lets)
  show (TLLetNoBody _ name) = "# let " <> show name

-- | TODO: Use https://hackage.haskell.org/package/prettyprinter
-- Not because it's easy, but because this looks horrible with longer functions.
-- And that's a "bad thing"^TM
instance Show (Expr' t Cursor) where
  show (Var _ name       ) = show name
  show (App _ e1 e2      ) = "(" <> show e1 <> " " <> show e2 <> ")"
  show (Lam _ _ name expr) = "\\" <> show name <> " -> " <> show expr
  show (LetIn _ name e1 e2) =
    "let " <> show name <> " = " <> show e1 <> " in\n" <> show e2
  show (If _ cond thn els) =
    "if " <> show cond <> " then " <> show thn <> " else " <> show els
  show (Literal c) =
    let spelling = BS.unpack $ cursorSpelling c
    in  if null spelling then "<" <> show (cursorKind c) <> ">" else spelling
  {-show (Ctor n e  ) = "(" <> show n <> " " <> show e <> ")"
  show (Elim k xs e1 e2) =
    "letelim "
      <> show k
      <> " "
      <> unwords (show <$> xs)
      <> " = "
      <> show e1
      <> " in "
      <> show e2-}
  show (Builtin _ be) = show be
  show _              = error "whoops"

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
