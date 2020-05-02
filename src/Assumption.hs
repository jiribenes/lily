{-# LANGUAGE DerivingStrategies #-}
module Assumption where

import           Type

import           Data.Coerce
import qualified Data.Set                      as S
import Data.List (foldl')

-- | Multimap of assumptions 
newtype Assumption t = Assumption { unAssumption :: [(Name, t)] }
    deriving stock (Eq, Ord, Show)

instance Semigroup (Assumption t) where
  Assumption as <> Assumption bs = Assumption $ as <> bs

instance Monoid (Assumption t) where
  mempty = empty

empty :: Assumption t
empty = Assumption []

singleton :: (Name, t) -> Assumption t
singleton (n, t) = Assumption [(n, t)]

extend :: Assumption t -> (Name, t) -> Assumption t
extend (Assumption a) (n, t) = Assumption ((n, t) : a)

extendMany :: Assumption t -> [(Name, t)] -> Assumption t
extendMany (Assumption a) xs = Assumption (xs <> a)

keys :: Assumption t -> [Name]
keys = map fst . (coerce :: Assumption t -> [(Name, t)])

-- TODO: more efficient impl?
keysSet :: Assumption t -> S.Set Name
keysSet = S.fromList . keys

member :: Name -> Assumption t -> Bool
member x = (x `S.member`) . keysSet

notMember :: Name -> Assumption t -> Bool
notMember x = not . member x

set :: Ord t => Assumption t -> S.Set (Name, t)
set (Assumption a) = S.fromList a

intersection :: Ord t => Assumption t -> Assumption t -> Assumption t
intersection as bs = Assumption . S.toList $ S.intersection (set as) (set bs)

intersectMany :: Ord t => [Assumption t] -> Assumption t
intersectMany as = Assumption . S.toList $ foldl' S.intersection S.empty (set <$> as)

remove :: Assumption t -> Name -> Assumption t
remove (Assumption a) x = Assumption $ filter ((/= x) . fst) a

removeMany :: Assumption t -> [Name] -> Assumption t
removeMany (Assumption a) xs =
  Assumption $ filter (\(x, _) -> x `notElem` xs) a

lookup :: Name -> Assumption t -> [t]
lookup x (Assumption a) = snd <$> filter ((== x) . fst) a