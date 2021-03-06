{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
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

import           Clang
import           Clang.OpParser
import           Name
import           Type.Type                      ( Type(..) )
import           Data.List                      ( find )

-- | All Lily Core expressions
--
-- Warning: The cursors are NOT injective! 
-- Multiple different expressions can have and indeed WILL have the same cursor.
data Expr' t c = Var !c !Name
               | App !c !(Expr' t c) !(Expr' t c)
               | Lam !c !Name !(Expr' t c)
               | LetIn !c !Name !(Expr' t c) !(Expr' t c)
               | Literal !c !t
               | If !c !(Expr' t c) !(Expr' t c) !(Expr' t c)
               | Builtin !c !BuiltinExpr
          deriving stock (Eq, Ord, Show, Functor)

-- | All Lily Core builtins
data BuiltinExpr = BuiltinBinOp !BinOp !Type !Type
                 | BuiltinUnOp !UnOp !Type !Type
                 | BuiltinMemberRef !Type  -- modelled after 'GHC.Records.HasField'
                 | BuiltinNew !Type
                 | BuiltinDelete
                 | BuiltinNewArray !Type
                 | BuiltinArraySubscript !Type !Type
                 | BuiltinAssign
                 | BuiltinUnit
                 | BuiltinNullPtr
                 | BuiltinThis !Type
  deriving stock (Eq, Show, Ord)

instance Pretty BuiltinExpr where
  pretty (BuiltinBinOp bop resultType opType) =
    "#builtin_binop_"
      <>  pretty (drop (length ("BinOp" :: String)) (show bop))
      <+> prettyTypeApplication resultType
      <+> prettyTypeApplication opType
  pretty (BuiltinUnOp uop resultType opType) =
    "#builtin_unop_"
      <>  pretty (drop (length ("UnOp" :: String)) (show uop))
      <+> prettyTypeApplication resultType
      <+> prettyTypeApplication opType
  pretty (BuiltinMemberRef fieldNameType) =
    "#builtin_memberref" <+> prettyTypeApplication fieldNameType
  pretty (BuiltinNew typ) = "#builtin_new" <+> prettyTypeApplication typ
  pretty BuiltinDelete    = "#builtin_delete"
  pretty (BuiltinNewArray typ) =
    "#builtin_new_array" <+> prettyTypeApplication typ
  pretty BuiltinAssign = "#builtin_assign"
  pretty (BuiltinArraySubscript arrayTyp subscriptTyp) =
    "#builtin_arrsubscript"
      <+> prettyTypeApplication arrayTyp
      <+> prettyTypeApplication subscriptTyp
  pretty BuiltinUnit       = "#builtin_unit"
  pretty BuiltinNullPtr    = "#builtin_nullptr"
  pretty (BuiltinThis typ) = "#builtin_this" <+> prettyTypeApplication typ

-- | A helper function for pretty-printing type applications
prettyTypeApplication :: Type -> PP.Doc ann
prettyTypeApplication typ@TAp{} = "@" <> PP.parens (pretty typ)
prettyTypeApplication typ       = "@" <> pretty typ

type Expr = Expr' Type Cursor
type CursorExpr t = Expr' t Cursor

-- | Takes an expression and peels off the initial 'Lam's
-- and gathers their names, leaving just the proper expression.
--
-- This is used for pretty printing a 'TopLevel' 'Let' expression.
gatherArguments :: Expr' t c -> ([Name], Expr' t c)
gatherArguments expr = go [] expr & _1 %~ reverse
 where
  go names (Lam _ n e) = go (n : names) e
  go names e           = (names, e)

-- | Every let declaration is either a function or a constructor
data LetKind = LetFunction | LetConstructor
  deriving stock (Eq, Ord, Show)

instance Pretty LetKind where
  pretty LetFunction    = "fun"
  pretty LetConstructor = "con"

-- | Represents a single top-level let declaration
data Let' t c = Let !c LetKind !Name !(Expr' t c)
  deriving stock (Eq, Ord, Show, Functor)

type Let = Let' Type Cursor

instance Pretty t => Pretty (Let' t Cursor) where
  pretty (Let _ k name expr) = pretty k <+> PP.align
    (PP.sep [pretty name <> prettyArgNames <+> "=", PP.hang 4 (pretty restExpr)]
    )
   where
    (argNames, restExpr) = gatherArguments expr
    prettyArgNames =
      if null argNames then "" else PP.space <> PP.hsep (pretty <$> argNames)

-- | Represents a single field of a struct
data StructField' t c = StructField !c !t !Name
  deriving stock (Eq, Ord, Show, Functor)

instance Pretty t => Pretty (StructField' t Cursor) where
  pretty (StructField _ t n) = pretty n <+> "::" <+> pretty t

type StructField = StructField' Type Cursor

type MethodNames = [Name]

-- | Represents a struct, @t@ is assumed to be a type constructor!
data Struct' t c = Struct !c !t !Name ![StructField' t c] ![ConstructorCursor] !MethodNames
  deriving stock (Eq, Show, Functor)

type Struct = Struct' Type Cursor

instance Pretty t => Pretty (Struct' t Cursor) where
  pretty (Struct _ _ name s _ _) =
    "struct" <+> pretty name <+> "=" <+> PP.align (braced $ pretty <$> s)
   where
    braced = PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt " }" "}") ", "

-- | Returns a field of given name if it exists in the given struct
findField :: Name -> Struct' t c -> Maybe (StructField' t c)
findField n (Struct _ _ _ fields _ _) = find (isThisYourFieldName n) fields
 where
  isThisYourFieldName name (StructField _ _ fieldName) = name == fieldName

-- | Checks if a struct contains a method
hasMethod :: Name -> Struct' t c -> Bool
hasMethod n (Struct c _ _ _ _ methods) = n `elem` methods

-- | A top-level declaration is either a single 'Let', a recursive group of 'Let', or a 'Struct'
data TopLevel' t c = TLLet !(Let' t c)
                   | TLLetRecursive !(NonEmpty (Let' t c))
                   | TLStruct !(Struct' t c)
  deriving stock (Eq, Show, Functor)
makePrisms ''TopLevel'

type TopLevel = TopLevel' Type Cursor
type Program = [TopLevel]

instance Pretty t => Pretty (TopLevel' t Cursor) where
  pretty (TLLet l) = pretty l
  pretty (TLLetRecursive ls) =
    "rec" <+> PP.align (PP.vsep $ NE.toList $ pretty <$> ls)
  pretty (TLStruct s) = pretty s

-- | Takes an expression and peels off the initial 'LetIn's
-- and gathers their names, leaving just the proper expression.
--
-- This is used for pretty printing 'Expr'essions.
gatherLets :: Expr' t c -> ([(Name, Expr' t c)], Expr' t c)
gatherLets expr = go [] expr & _1 %~ reverse
 where
  go lets (LetIn _ name e1 e2) = go ((name, e1) : lets) e2
  go lets e                    = (lets, e)

instance Pretty t => Pretty (Expr' t Cursor) where
  pretty (Var _ name         ) = pretty name
  pretty (App _ e1   e2@App{}) = pretty e1 <+> PP.parens (pretty e2)
  pretty (App _ e1   e2      ) = pretty e1 <+> pretty e2
  pretty (Lam _ name expr    ) = "\\" <> pretty name <+> "->" <+> pretty expr
  pretty expr@LetIn{}          = PP.align
    $ PP.vsep [PP.vsep (prettyLet <$> lets), PP.vsep ["in" <+> pretty restExpr]]
   where
    (lets, restExpr) = gatherLets expr
    prettyLet (n, e) = "let" <+> pretty n <+> "=" <+> pretty e
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

-- | A lens from @a@ to a 'Cursor'
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

-- this instance is a bit obvious, but...
instance HasCursor Cursor where
  cursorL = lens id const

instance HasCursor (Let' t Cursor) where
  cursorL = lens getter setter
   where
    getter :: Let' t Cursor -> Cursor
    getter (Let c _ _ _) = c
    setter :: Let' t Cursor -> Cursor -> Let' t Cursor
    setter l newCursor = l $> newCursor

instance HasCursor TopLevel where
  cursorL = lens getter setter
   where
    getter :: TopLevel -> Cursor
    getter = \case
      TLLet          l        -> view cursorL l
      TLLetRecursive (l :| _) -> view cursorL l
      TLStruct       s        -> view cursorL s
    setter :: TopLevel -> Cursor -> TopLevel
    setter c newCursor = c $> newCursor

instance HasCursor (Struct' t Cursor) where
  cursorL = lens (\(Struct c _ _ _ _ _) -> c) ($>)

-- | Quick constructor for 'BuiltinUnit'
unit :: Cursor -> Expr
unit c = Builtin c BuiltinUnit

-- | A lens from @a@ to a 'Name'
class HasName a where
  nameL :: Lens' a Name

instance HasName (Let' t c) where
  nameL = lens getter setter
   where
    getter :: Let' t c -> Name
    getter (Let _ _ n _) = n
    setter (Let c k _ e) n = Let c k n e
