{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-} -- used for (~) constraint only

module MonadFresh
  ( FreshState
  , initialFreshState
  , MonadFresh
  , fresh
  , setFresh
  , getFresh
  , FreshT
  , evalFreshT
  , runFreshT
  , Fresh
  , evalFresh
  , runFresh
  )
where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.RWS
import           Control.Monad.Trans.Maybe
import           Control.Applicative

import           Control.Lens
import qualified Data.Text as T
import qualified Data.List.NonEmpty            as NE
import           Data.Kind                      ( Type )

import Type (Name(..))

newtype FreshState n = FreshState { unFresh :: NE.NonEmpty n }

-- makes a lens `_fresh` for getting the contents of `FreshState`
makeLensesFor [("unFresh", "_fresh")] ''FreshState

-- | Equivalent to `_head` but for `NonEmpty`
-- | It's an actual lawful `Lens` instead of a `Traversal`!
_neHead :: Lens' (NE.NonEmpty a) a
_neHead f (a NE.:| as) = (NE.:| as) <$> f a

initialFreshState :: FreshState Name
initialFreshState = FreshState { unFresh = Name . ("t" <>) . T.pack . show <$> NE.iterate (+ 1) 0 }

-- | Monad transformer for `FreshState`
newtype FreshT n (m :: Type -> Type) a = FreshT { unFreshT :: StateT (FreshState n) m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadPlus, Alternative, MonadReader r, MonadWriter w, MonadState (FreshState n), MonadIO)

evalFreshT :: Monad m => FreshT n m a -> FreshState n -> m a
evalFreshT = evalStateT . unFreshT
{-# INLINE evalFreshT #-}

runFreshT :: FreshT n m a -> FreshState n -> m (a, FreshState n)
runFreshT = runStateT . unFreshT
{-# INLINE runFreshT #-}

-- | Monad for getting a supply of fresh identifiers `n`
-- | Should support `get-set`, `set-set` and `set-get` laws like lenses (!)
class (Monad m) => MonadFresh n m where
  -- | Get a fresh identifier
  fresh :: m n

  -- | Set a new supply
  setFresh :: FreshState n -> m ()

  -- | Get the current supply
  getFresh :: m (FreshState n)

  -- technique from https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/ 
  -- | `fresh` for automatically deriving transformers
  default fresh :: (MonadTrans t, MonadFresh n1 m1, m ~ t m1, n ~ n1) => m n
  fresh = lift fresh
  {-# INLINE fresh #-}

  -- | `setFresh` for automatically deriving transformers
  default setFresh :: (MonadTrans t, MonadFresh n1 m1, m ~ t m1, n ~ n1) => FreshState n -> m ()
  setFresh = lift . setFresh
  {-# INLINE setFresh #-}

  -- | `getFresh` for automatically deriving transformers
  default getFresh :: (MonadTrans t, MonadFresh n1 m1, m ~ t m1, n ~ n1) => m (FreshState n)
  getFresh = lift getFresh
  {-# INLINE getFresh #-}

-- the actual implementation
instance Monad m => MonadFresh n (FreshT n m) where
  fresh = FreshT $ do
    _fresh <%= NE.fromList . NE.tail
    use (_fresh . _neHead)
  {-# INLINE fresh #-}

  setFresh = FreshT . put
  {-# INLINE setFresh #-}

  getFresh = FreshT get
  {-# INLINE getFresh #-}

-- automatically derived transformer boilerplate
instance MonadFresh n m => MonadFresh n (MaybeT m)
instance MonadFresh n m => MonadFresh n (StateT s m)
instance MonadFresh n m => MonadFresh n (ReaderT r m)
instance (Monoid w, MonadFresh n m) => MonadFresh n (WriterT w m)
instance (Monoid w, MonadFresh n m) => MonadFresh n (RWST r w s m)

instance MonadError e m => MonadError e (FreshT n m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}

-- this function has been fully written by hole-driven programming
  catchError m h = FreshT $ catchError (unFreshT m) (unFreshT . h)
  {-# INLINE catchError #-}

-- | Type synonym for using Fresh only 
type Fresh n = FreshT n Identity

-- | `evalFreshT` specialized for (m ~ Identity)
evalFresh :: Fresh n a -> FreshState n -> a
evalFresh n = runIdentity . evalFreshT n
{-# INLINE evalFresh #-}

-- | `runFreshT` specialized for (m ~ Identity)
runFresh :: Fresh n a -> FreshState n -> (a, FreshState n)
runFresh n = runIdentity . runFreshT n
{-# INLINE runFresh #-}