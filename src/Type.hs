{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Type where

import           Data.ByteString.Char8          ( ByteString )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import qualified Data.Set                      as S
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

newtype Name = Name { unName :: Text }
  deriving newtype (Eq, Ord, IsString, Pretty)

instance Show Name where
  show = T.unpack . unName

nameFromBS :: ByteString -> Name
nameFromBS = Name . decodeUtf8

data TVar = TV Name Kind deriving stock (Show, Eq, Ord)

instance Pretty TVar where
  pretty (TV n StarKind) = pretty n
  pretty (TV n k       ) = PP.parens $ pretty n <+> "::" <+> pretty k

data TCon = TC Name Kind deriving stock (Show, Eq, Ord)

instance Pretty TCon where
  pretty (TC n StarKind) = pretty n
  pretty (TC n k       ) = PP.parens $ pretty n <+> "::" <+> pretty k

-- | Types are either type variables, type constructors or a type applied to another
data Type = TVar TVar
          | TCon TCon
          | TAp Type Type
          deriving stock (Eq, Show, Ord)

instance Pretty Type where
  pretty = prettyType mempty

prettyType :: S.Set Pred -> Type -> PP.Doc ann
prettyType ps = \case
  TVar (TV n _) -> pretty n
  TCon (TC n _) -> pretty n
  LinArrow a b  -> prettyLeft ps a <+> "-o>" <+> prettyType ps b
  UnArrow  a b  -> prettyLeft ps a <+> "-●>" <+> prettyType ps b
  Arrow    a b  -> prettyLeft ps a <+> "->" <+> prettyType ps b
  VariableArrow (TVar (TV n _)) a b ->
    prettyLeft ps a <+> "-" <> PP.braces (pretty n) <> ">" <+> prettyType ps b
  TAp a@TAp{} b -> PP.parens (prettyType ps a) <+> prettyType ps b
  TAp a       b -> prettyType ps a <+> prettyType ps b
 where
  prettyLeft ps a = maybeParenArrow isFunction a (prettyType ps a)
  isFunction f = PFun f `S.member` ps

  maybeParenArrow :: (Type -> Bool) -> Type -> PP.Doc ann -> PP.Doc ann
  maybeParenArrow p f doc = if isArrow p f then PP.parens doc else doc

-- | Kind is the type of type
-- needed for type constructors
data Kind = StarKind
          | ArrowKind Kind Kind
          deriving stock (Eq, Show, Ord)

instance PP.Pretty Kind where
  pretty StarKind = "Type"
  pretty (ArrowKind a@ArrowKind{} b) =
    PP.parens (pretty a) <+> "->" <+> pretty b
  pretty (ArrowKind a b) = pretty a <+> "->" <+> pretty b

typeUnit :: Type
typeUnit = TCon $ TC "Unit" StarKind
typeInt :: Type
typeInt = TCon $ TC "Int" StarKind
typeUInt :: Type
typeUInt = TCon $ TC "UInt" StarKind
typeBool :: Type
typeBool = TCon $ TC "Bool" StarKind
typeChar :: Type
typeChar = TCon $ TC "Char" StarKind
conPtr :: TCon
conPtr = TC "Ptr" $ ArrowKind StarKind StarKind
typePtr :: Type
typePtr = TCon conPtr
typePtrOf :: Type -> Type
typePtrOf t = typePtr `TAp` t
conList :: TCon
conList = TC "[]" $ ArrowKind StarKind StarKind
typeList :: Type
typeList = TCon conList
arrowKind :: Kind
arrowKind = ArrowKind StarKind (ArrowKind StarKind StarKind)
conArrow :: TCon
conArrow = TC "->" arrowKind
conLinArrow :: TCon
conLinArrow = TC "-o>" arrowKind
conUnArrow :: TCon
conUnArrow = TC "-●>" arrowKind
typeArrow :: Type
typeArrow = TCon conArrow
typeLinArrow :: Type
typeLinArrow = TCon conLinArrow
typeUnArrow :: Type
typeUnArrow = TCon conUnArrow

-- | (typeclass) Predicate 
data Pred = IsIn Name (NonEmpty Type) deriving stock (Eq, Show, Ord)

instance Pretty Pred where
  pretty (PGeq x y ) = pretty x <+> "⩾" <+> pretty y
  pretty (IsIn n ts) = pretty n <+> PP.sep (NE.toList $ pretty <$> ts)

-- constructors for the four most used predicates
pattern PFun :: Type -> Pred
pattern PFun x <- IsIn "Fun" [x] where PFun x = IsIn "Fun" [x]
pattern PUn :: Type -> Pred
pattern PUn x <- IsIn "Un" [x] where PUn x = IsIn "Un" [x]
pattern PGeq :: Type -> Type -> Pred
pattern PGeq x y <- IsIn "Geq" [x, y] where PGeq x y = IsIn "Geq" [x, y]
pattern PNum :: Type -> Pred
pattern PNum x <- IsIn "Num" [x] where PNum x = IsIn "Num" [x]

pattern LinArrow :: Type -> Type -> Type
pattern LinArrow a b <- (extractSpecificArrow (== conLinArrow) -> Just (a, b)) 
  where LinArrow a b = TCon conLinArrow `TAp` a `TAp` b

pattern UnArrow :: Type -> Type -> Type
pattern UnArrow a b <- (extractSpecificArrow (== conUnArrow) -> Just (a, b))
  where UnArrow a b = TCon conUnArrow `TAp` a `TAp` b

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b <- (extractSpecificArrow (== conArrow) -> Just (a, b))
  where Arrow a b = TCon conArrow `TAp` a `TAp` b

-- This pattern is unidirectional only on purpose!
pattern SpecificArrow :: Type -> Type -> Type
pattern SpecificArrow a b <- (extractSpecificArrow isSpecificArrow -> Just (a, b))

pattern VariableArrow :: Type -> Type -> Type -> Type
pattern VariableArrow f a b <- (extractVariableArrow -> Just (f, a, b))

extractVariableArrow :: Type -> Maybe (Type, Type, Type)
extractVariableArrow (f@TVar{} `TAp` a `TAp` b) = Just (f, a, b)
extractVariableArrow _                          = Nothing

isSpecificArrow :: TCon -> Bool
isSpecificArrow = flip elem arrows
 where
  arrows :: [TCon]
  arrows = [conLinArrow, conUnArrow, conArrow]

extractSpecificArrow :: (TCon -> Bool) -> Type -> Maybe (Type, Type)
extractSpecificArrow p (TCon c `TAp` a `TAp` b) =
  if p c then Just (a, b) else Nothing
extractSpecificArrow _ _ = Nothing

isArrow :: (Type -> Bool) -> Type -> Bool
isArrow p (f@TVar{} `TAp` _ `TAp` _) = p f
isArrow _ t = isJust $ extractSpecificArrow isSpecificArrow t

-- | Qualified `a` is an `a` with a list of predicates
data Qual a = [Pred] :=> a deriving stock (Eq, Show, Ord)

instance Pretty (Qual Type) where
  pretty (preds :=> t) =
    PP.tupled (pretty <$> preds) <+> "=>" <+> prettyType (S.fromList preds) t

isOnlyFun :: S.Set Pred -> Type -> Bool
isOnlyFun ps t = S.singleton (PFun t) == relevantPreds ps t
relevantPreds :: S.Set Pred -> Type -> S.Set Pred
relevantPreds ps t = (\(IsIn _ xs) -> t `elem` xs) `S.filter` ps

-- | A type scheme is a list of type variables and a qualified type
data Scheme = Forall [TVar] (Qual Type) deriving stock (Eq, Show, Ord)

instance Pretty Scheme where -- doesn't use the Pretty (Qual Type) instance to be prettier!
  pretty (Forall tvs (preds :=> t)) = PP.align (PP.sep inner)
   where
    inner :: [PP.Doc ann]
    inner =
      [ "forall" <+> PP.align (PP.sep (pretty <$> tvs))
      , "." <+> PP.tupled (pretty <$> preds)
      , "=>" <+> prettyType (S.fromList preds) t
      ]

-- | Substitution is a map from type variables to actual types
newtype Subst = Subst { unSubst :: M.Map TVar Type}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance Pretty Subst where
  pretty (Subst m) = PP.list (prettyOne <$> M.toList m)
    where prettyOne (tv, ty) = pretty tv <+> ":=" <+> pretty ty

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) =
  Subst $ M.map (apply (Subst s1)) s2 `M.union` s1

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable TVar where
  apply (Subst s) a = tv
   where
    t         = TVar a
    (TVar tv) = M.findWithDefault t a s -- this is fine by construction!

instance Substitutable Type where
  apply _         con@TCon{}       = con
  apply (Subst s) tv@(TVar x     ) = M.findWithDefault tv x s
  apply s         (   t1 `TAp` t2) = apply s t1 `TAp` apply s t2

instance Substitutable Pred where
  apply s (IsIn x t) = IsIn x (apply s t)

-- | Note: Cannot substitute for quantified variables!
instance Substitutable Scheme where
  apply (Subst s) (Forall as qt) = Forall as (apply s' qt)
    where s' = Subst (foldr M.delete s as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance Substitutable a => Substitutable (NonEmpty a) where
  apply = fmap . apply

instance (Ord a, Substitutable a) => Substitutable (S.Set a) where
  apply = S.map . apply

instance Substitutable a => Substitutable (Qual a) where
  apply s (preds :=> t) = apply s preds :=> apply s t

instance Substitutable a => Substitutable (M.Map k a) where
  apply s = M.map (apply s)

class FreeTypeVars a where
  ftv   :: a -> S.Set TVar

instance FreeTypeVars Type where
  ftv TCon{}        = S.empty
  ftv (TVar x     ) = S.singleton x
  ftv (t1 `TAp` t2) = ftv t1 `S.union` ftv t2

instance FreeTypeVars TVar where
  ftv = S.singleton

instance FreeTypeVars Scheme where
  ftv (Forall as qt) = ftv qt `S.difference` S.fromList as

instance FreeTypeVars a => FreeTypeVars (Qual a) where
  ftv (_ :=> t) = ftv t

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = foldr (S.union . ftv) S.empty

-- transitive instance for ordered sets
instance FreeTypeVars a => FreeTypeVars (S.Set a) where
  ftv = foldr (S.union . ftv) S.empty

class ActiveTypeVars a where
  atv :: a -> S.Set TVar

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr (S.union . atv) S.empty

-- transitive instance for ordered sets
instance ActiveTypeVars a => ActiveTypeVars (S.Set a) where
  atv = foldr (S.union . atv) S.empty

-- | Get a kind from a type
typeKind :: Type -> Kind
typeKind (TCon (TC _ k)) = k
typeKind (TVar (TV _ k)) = k
typeKind (TAp a _      ) = case typeKind a of
  ArrowKind _ k -> k
  k             -> k
