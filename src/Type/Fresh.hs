{-# LANGUAGE FlexibleContexts #-}
module Type.Fresh ( freshType, freshTypeAndTVar, instantiate ) where

import qualified Data.Map as M

import Name
import Type.Type
import Control.Monad.Fresh

freshType :: MonadFresh Name m => Kind -> m Type
freshType kind = do
  name <- fresh
  let tv = TV name kind
  pure $ TVar tv

freshTypeAndTVar :: MonadFresh Name m => Kind -> m (Type, TVar)
freshTypeAndTVar kind = do
  name <- fresh
  let tv = TV name kind
  pure (TVar tv, tv)

instantiate :: MonadFresh Name m => Scheme -> m (Type, [Pred])
instantiate (Forall xs qt) = do
  xs' <- traverse (\(TV _ k) -> freshType k) xs
  let sub         = Subst $ M.fromList $ zip xs xs'
      preds :=> t = sub `apply` qt

  pure (t, preds)