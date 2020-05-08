{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unify where

import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Type

data UnificationError = InfiniteType TVar Type
                      | UnificationFail Type Type
                      | KindMismatch Type Type
                      | ConstructorNotFound Name
                      | Unsatisfied [Pred]
                      deriving stock (Eq, Ord)

instance Pretty UnificationError where
  pretty (UnificationFail a b) = PP.align
    (PP.vsep -- TODO: kind is mostly for debug purposes (?)
      [ "Cannot unify:"
      , PP.indent 4 $ PP.hsep [pretty a, PP.parens $ "of kind:" <+> pretty (typeKind a)]
      , "with:"
      , PP.indent 4 $ PP.hsep [pretty b, PP.parens $ "of kind:" <+> pretty (typeKind b)]
      ]
    )
  pretty (KindMismatch a b) = PP.align
    (PP.vsep
      [ "Kind mismatch between:" <+> pretty a <+> "and" <+> pretty b
      , "because: " <+> pretty (typeKind a) <+> "/=" <+> pretty (typeKind b)
      ]
    )
  pretty (InfiniteType (TV a _) b) =
    "Cannot construct infinite type:" <+> pretty a <+> "=" <+> pretty b
  pretty (ConstructorNotFound n) = "Cannot find constructor:" <+> pretty n
  pretty (Unsatisfied preds) =
    "Cannot satisfy the following predicates: "
      <+> PP.indent 4 (PP.vsep $ pretty <$> preds)

unifies :: MonadError UnificationError m => Type -> Type -> m Subst
unifies t1 t2 | t1 == t2                   = pure emptySubst
              | typeKind t1 /= typeKind t2 = throwError $ KindMismatch t1 t2
unifies (TCon c1) (TCon c2) | c1 == c2 = pure emptySubst
unifies (TVar tv)   t                  = tv `bind` t
unifies t           (TVar tv  )        = tv `bind` t
unifies (TAp t1 t2) (TAp t3 t4)        = unifyMany [t1, t2] [t3, t4]
unifies t1          t2                 = throwError $ UnificationFail t1 t2

occursCheck :: FreeTypeVars a => TVar -> a -> Bool
occursCheck tv t = tv `S.member` ftv t

bind :: MonadError UnificationError m => TVar -> Type -> m Subst
bind tv@(TV _ k) t | t == TVar tv     = pure emptySubst
                   | typeKind t /= k  = throwError $ KindMismatch (TVar tv) t
                   | occursCheck tv t = throwError $ InfiniteType tv t
                   | otherwise        = pure $ Subst $ M.singleton tv t

-- | This function requires the two lists to be equal in length.
-- | Please make sure that they are or else this program throws a runtime error!
unifyMany :: MonadError UnificationError m => [Type] -> [Type] -> m Subst
unifyMany []       []       = pure emptySubst
unifyMany (t : ts) (u : us) = do
  sub1 <- unifies t u
  sub2 <- unifyMany (sub1 `apply` ts) (sub1 `apply` us)
  pure (sub2 `compose` sub1)
unifyMany _ _ = error "unifyMany: the length of lists differs!"
