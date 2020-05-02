{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Type where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Text                      ( Text )

import           Data.List                      ( intercalate )

type Name = Text

data TVar = TV Name Kind deriving stock (Show, Eq, Ord)
data TCon = TC Name Kind deriving stock (Show, Eq, Ord)

-- | Types are either type variables, type constructors or a type applied to another
data Type = TVar TVar
          | TCon TCon
          | TAp Type Type
          deriving stock (Eq, Ord)

-- | Kind is the type of type
-- needed for type constructors
data Kind = StarKind
          | ArrowKind Kind Kind
          deriving stock (Eq, Ord)

instance Show Kind where
  show StarKind        = "Type"
  show (ArrowKind StarKind StarKind) = "Type -> Type"
  show (ArrowKind a b@ArrowKind{}) = show a <> " -> " <> show b
  show (ArrowKind a b) = "(" <> show a <> " -> " <> show b <> ")"

instance Show Type where
  show (TVar (TV a _)) = "'" <> show a
  show (TCon (TC c _)) = show c
  show (TCon c `TAp` a `TAp` b)
    | c == conArrow    = "(" <> show a <> " -> " <> show b <> ")"
    | c == conLinArrow = "(" <> show a <> " -o> " <> show b <> ")"
    | c == conUnArrow  = "(" <> show a <> " -●> " <> show b <> ")"
  show (TAp a b) = "(" <> show a <> " " <> show b <> ")"

typeUnit :: Type
typeUnit = TCon $ TC "Unit" StarKind
typeInt :: Type
typeInt = TCon $ TC "Int" StarKind
typeBool :: Type
typeBool = TCon $ TC "Bool" StarKind
typeChar :: Type
typeChar = TCon $ TC "Char" StarKind
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
data Pred = IsIn Name [Type] deriving stock (Eq, Ord)

instance Show Pred where
  show (IsIn "Geq" [x, y]) = show x <> " ⩾ " <> show y
  show (IsIn n     ts    ) = show n <> " " <> unwords (show <$> ts)

-- constructors for the four most used predicates
predFun :: Type -> Pred
predFun x = IsIn "Fun" [x]
predGeq :: Type -> Type -> Pred
predGeq x y = IsIn "Geq" [x, y]
predUn :: Type -> Pred
predUn x = IsIn "Un" [x]
predNum :: Type -> Pred
predNum x = IsIn "Num" [x]

-- | Qualified `a` is an `a` with a list of predicates
data Qual a = [Pred] :=> a deriving stock (Eq, Ord)

instance Show (Qual Type) where
  show (preds :=> t) =
    "("
      <> (intercalate ", " . fmap show $ preds)
      <> ") => "
      <> showPretty (S.fromList preds) t

-- | Helper function for showing a type nicely
showPretty :: S.Set Pred -> Type -> String
showPretty _ (TVar (TV a _)) = "'" <> show a
showPretty ps ((TCon c) `TAp` a `TAp` b)
  | c == conArrow
  = "(" <> showPretty ps a <> " -> " <> showPretty ps b <> ")"
  | c == conLinArrow
  = "(" <> showPretty ps a <> " -o> " <> showPretty ps b <> ")"
  | c == conUnArrow
  = "(" <> showPretty ps a <> " -●> " <> showPretty ps b <> ")"
showPretty ps (f `TAp` a `TAp` b)
  | isOnlyFun ps f
  = "(" <> showPretty ps a <> " -> " <> showPretty ps b <> ")"
  | predFun f `S.member` ps
  = "("
    <> showPretty ps a
    <> " -"
    <> showPretty ps f
    <> "> "
    <> showPretty ps b
    <> ")"
showPretty _ (TCon (TC c _)) = show c
showPretty ps (TAp (TCon c) b)
  | c == conList = "[" <> showPretty ps b <> "]"
showPretty ps (TAp a b)
  = "(" <> showPretty ps a <> " " <> showPretty ps b <> ")"


isOnlyFun :: S.Set Pred -> Type -> Bool
isOnlyFun ps t = S.singleton (predFun t) == relevantPreds ps t
relevantPreds :: S.Set Pred -> Type -> S.Set Pred
relevantPreds ps t = (\(IsIn _ xs) -> t `elem` xs) `S.filter` ps

-- | A type scheme is a list of type variables and a qualified type
data Scheme = Forall [TVar] (Qual Type) deriving stock (Eq, Ord)

instance Show Scheme where
  show (Forall tvs qt) =
    "forall "
      <> unwords ((\(TV a k) -> show a <> showKind k) <$> tvs)
      <> " . "
      <> show qt
   where
    showKind k = case k of
      StarKind      -> ""
      ArrowKind _ _ -> ": " <> show k


-- | Substitution is a map from type variables to actual types
newtype Subst = Subst (M.Map TVar Type) deriving stock (Eq, Ord, Show)
                                        deriving newtype (Semigroup, Monoid)

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
    (TVar tv) = M.findWithDefault t a s

instance Substitutable Type where
  apply _         con@TCon{}       = con
  apply (Subst s) tv@(TVar x     ) = M.findWithDefault tv x s
  apply s         (   t1 `TAp` t2) = apply s t1 `TAp` apply s t2

instance Substitutable Pred where
  apply s (IsIn x t) = IsIn x (apply s t)

instance Substitutable Scheme where
  apply (Subst s) (Forall as qt) = Forall as (apply s' qt)
    where s' = Subst (foldr M.delete s as)

instance Substitutable a => Substitutable [a] where
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