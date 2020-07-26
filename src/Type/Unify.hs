{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Unify
  ( UnificationError(..)
  , unifies
  , unifyMany
  , onewayUnifies
  )
where

import           Control.Monad.Except
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Type.Type
import           Type.Fresh
import           Name
import           Control.Monad.Fresh

data UnificationError = InfiniteType TVar Type
                      | UnificationFail Type Type
                      | KindMismatch Type Type
                      | ConstructorNotFound Name
                      | Unsatisfied [Pred]
                      deriving stock (Eq, Ord, Show)

instance Pretty UnificationError where
  pretty (UnificationFail a b) = PP.align
    (PP.vsep -- TODO: kind is mostly for debug purposes (?)
      [ "Cannot unify:"
      , PP.indent 4
        $ PP.hsep [pretty a, PP.parens $ "of kind:" <+> pretty (typeKind a)]
      , "with:"
      , PP.indent 4
        $ PP.hsep [pretty b, PP.parens $ "of kind:" <+> pretty (typeKind b)]
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
-- these rules are special for mutable references
-- we should fix it up to say something like
--
--   MR t1        t2
--   ---------------
--     t2 := MR t1
-- 
-- so that it propagates
-- unifies a@(MutRef t1) t2 | tracePretty (a, t2) True =
--   t1 `unifies` t2
-- unifies t1 b@(MutRef t2) | tracePretty (t1, b) True =
--   t1 `unifies` t2

-- TODO: solve multiple layers of mutable references
-- how do we solve that properly? (mutable reborrows)

-- catch-all clause
unifies t1          t2                 = throwError $ UnificationFail t1 t2

wrapIn :: MonadError UnificationError m => (Type -> Type) -> m Subst -> m Subst
wrapIn f s = do
  Subst m <- s
  pure $ Subst $ M.map f m

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

-- | Local-only function for a cheap and dirty instantiation
dirtyInstantiate :: Monad m => Scheme -> m (Type, [Pred])
dirtyInstantiate = flip evalFreshT initialFreshState . instantiate

onewayUnifies
  :: MonadError UnificationError m => Scheme -> Type -> m (Subst, [Pred])
onewayUnifies sch t2 = do
  (t1, preds) <- dirtyInstantiate sch
  sub         <- t1 `locallyUnifies` t2
  pure $ (sub, sub `apply` preds)
 where
  locallyUnifies t1 t2
    | t1 == t2                   = pure emptySubst
    | typeKind t1 /= typeKind t2 = throwError $ KindMismatch t1 t2
  locallyUnifies (TCon c1) (TCon c2) | c1 == c2 = pure emptySubst
  locallyUnifies (TVar tv)   t                  = tv `bind` t
  locallyUnifies t           (TVar tv  )        = tv `bind` t
  locallyUnifies (TAp t1 t2) (TAp t3 t4) = locallyUnifyMany [t1, t2] [t3, t4]
  locallyUnifies t1          (LRef t2  )        = locallyUnifies t1 t2
  locallyUnifies t1          (RRef t2  )        = locallyUnifies t1 t2
  locallyUnifies (LRef t1)   t2                 = locallyUnifies t1 t2
  locallyUnifies (RRef t1)   t2                 = locallyUnifies t1 t2
  locallyUnifies t1 t2 = throwError $ UnificationFail t1 t2

  locallyUnifyMany []       []       = pure emptySubst
  locallyUnifyMany (t : ts) (u : us) = do
    sub1 <- locallyUnifies t u
    sub2 <- locallyUnifyMany (sub1 `apply` ts) (sub1 `apply` us)
    pure (sub2 `compose` sub1)
  locallyUnifyMany _ _ = error "locallyUnifyMany: the length of lists differs!"
