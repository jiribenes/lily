{-# LANGUAGE FlexibleContexts #-}
module Type.Fresh ( freshType, freshTypeAndTVar, instantiate ) where

import qualified Data.Map as M

import Name
import Type.Type
import Control.Monad.Fresh

-- | Creates a fresh type of a specified 'Kind'
freshType :: MonadFresh Name m => Kind -> m Type
freshType kind = do
  name <- fresh
  let tv = TV name kind
  pure $ TVar tv

-- | Creates a fresh type variable and type of a specified 'Kind'
freshTypeAndTVar :: MonadFresh Name m => Kind -> m (Type, TVar)
freshTypeAndTVar kind = do
  name <- fresh
  let tv = TV name kind
  pure (TVar tv, tv)

-- | Instantiates a polytype ('Scheme')
instantiate :: MonadFresh Name m => Scheme -> m (Type, [Pred])
instantiate (Forall xs qt) = do
  xs' <- traverse (\(TV _ k) -> freshType k) xs
  let sub         = Subst $ M.fromList $ zip xs xs'
      preds :=> t = sub `apply` qt

  pure (t, preds)