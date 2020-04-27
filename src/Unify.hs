{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Unify where

import           Type

import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           Control.Monad.Except

data UnificationError = InfiniteType TVar Type
                      | UnificationFail Type Type
                      | KindMismatch TVar Type
                      | ConstructorNotFound Name
                      | Unsatisfied [Pred]
                      deriving stock (Eq, Ord)

instance Show UnificationError where
  show (UnificationFail a b) =
    unlines ["Cannot unify: ", "\t\t" <> show a, " with: ", "\t\t" <> show b]
  show (KindMismatch (TV a k) b) =
    "Kind mismatch: "
      <> show a
      <> " and "
      <> show b
      <> ", because "
      <> show k
      <> " /= "
      <> show (typeKind b)
  show (InfiniteType (TV a _) b) =
    "Cannot construct infinite type: " <> show a <> " = " <> show b
  show (ConstructorNotFound n) = "Cannot find constructor: " <> show n
  show (Unsatisfied preds) =
    unlines
      $ "Cannot satisfy the following predicates: "
      : (("\t\t" <>) . show <$> preds)

unifies :: MonadError UnificationError m => Type -> Type -> m Subst
unifies t1 t2 | t1 == t2               = pure emptySubst
unifies (TCon c1) (TCon c2) | c1 == c2 = pure emptySubst
unifies (TVar tv)   t                  = tv `bind` t
unifies t           (TVar tv  )        = tv `bind` t
unifies (TAp t1 t2) (TAp t3 t4)        = unifyMany [t1, t2] [t3, t4]
unifies t1          t2                 = throwError $ UnificationFail t1 t2

occursCheck :: FreeTypeVars a => TVar -> a -> Bool
occursCheck tv t = tv `S.member` ftv t

bind :: MonadError UnificationError m => TVar -> Type -> m Subst
bind tv@(TV _ k) t | t == TVar tv     = pure emptySubst
                   | typeKind t /= k  = throwError $ KindMismatch tv t
                   | occursCheck tv t = throwError $ InfiniteType tv t
                   | otherwise        = pure $ Subst $ M.singleton tv t

unifyMany :: MonadError UnificationError m => [Type] -> [Type] -> m Subst
unifyMany []       []       = pure emptySubst
unifyMany (t : ts) (u : us) = do
  sub1 <- unifies t u
  sub2 <- unifyMany (sub1 `apply` ts) (sub1 `apply` us)
  pure (sub2 `compose` sub1)  