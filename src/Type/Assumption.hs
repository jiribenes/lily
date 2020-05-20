{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Type.Assumption
  ( Assumption
  , empty
  , singleton
  , extend
  , extendMany
  , keys
  , keysSet
  , values
  , member
  , notMember
  , toSet
  , intersection
  , intersectMany
  , remove
  , removeMany
  , Type.Assumption.lookup
  )
where

import           Data.Coerce
import qualified Data.Set                      as S
import           Data.List                      ( foldl' )
import           Data.Containers.ListUtils      ( nubOrd )

import           Name

-- | Multimap of assumptions 
newtype Assumption t = Assumption { unAssumption :: [(Name, t)] }
    deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

empty :: Assumption t
empty = mempty

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

values :: Assumption t -> [t]
values = map snd . (coerce :: Assumption t -> [(Name, t)])

member :: Name -> Assumption t -> Bool
member x = (x `S.member`) . keysSet

notMember :: Name -> Assumption t -> Bool
notMember x = not . member x

toSet :: Ord t => Assumption t -> S.Set (Name, t)
toSet (Assumption a) = S.fromList a

intersection :: Ord t => Assumption t -> Assumption t -> Assumption t
intersection as bs =
  Assumption . S.toList $ S.intersection (toSet as) (toSet bs)

intersectMany :: Ord t => [Assumption t] -> Assumption t
intersectMany as =
  Assumption . S.toList $ foldl' S.intersection S.empty (toSet <$> as)

remove :: Assumption t -> Name -> Assumption t
remove (Assumption a) x = Assumption $ filter ((/= x) . fst) a

removeMany :: Assumption t -> [Name] -> Assumption t
removeMany (Assumption a) xs =
  Assumption $ filter (\(x, _) -> x `notElem` xs) a

lookup :: Name -> Assumption t -> [t]
lookup x (Assumption a) = snd <$> filter ((== x) . fst) a
