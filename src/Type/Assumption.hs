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

import           Name

-- | Multimap of assumptions
newtype Assumption t = Assumption { unAssumption :: [(Name, t)] }
    deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

-- | Neutral element of 'Assumption', an empty assumption set
empty :: Assumption t
empty = mempty

-- | Create an 'Assumption' from a single assumption
singleton :: (Name, t) -> Assumption t
singleton (n, t) = Assumption [(n, t)]

-- | Adds a new assumption into 'Assumption'
extend :: Assumption t -> (Name, t) -> Assumption t
extend (Assumption a) (n, t) = Assumption ((n, t) : a)

-- | Adds many new assumptions into 'Assumption'
extendMany :: Assumption t -> [(Name, t)] -> Assumption t
extendMany (Assumption a) xs = Assumption (xs <> a)

-- | Returns the domain of the given 'Assumption'
keys :: Assumption t -> [Name]
keys = map fst . (coerce :: Assumption t -> [(Name, t)])

-- | Returns the domain of the given 'Assumption' as a set
keysSet :: Assumption t -> S.Set Name
keysSet = S.fromList . keys

-- | Returns the range of the given 'Assumption'
values :: Assumption t -> [t]
values = map snd . (coerce :: Assumption t -> [(Name, t)])

-- | Checks if a 'Name' is present in 'Assumption'
member :: Name -> Assumption t -> Bool
member x = (x `S.member`) . keysSet

-- | Checks if a 'Name' is not present in 'Assumption'
notMember :: Name -> Assumption t -> Bool
notMember x = not . member x

-- | Converts the 'Assumption' multiset into a set
toSet :: Ord t => Assumption t -> S.Set (Name, t)
toSet (Assumption a) = S.fromList a

-- | This is not a normal intersection!
--
-- It has a key property that @(x, t)@ is in the resulting assumption multiset
-- iff @x@ is in both assumption sets in the input.
--
-- This corresponds closely to Morris' operation where he
-- restricts the typing context \Gamma to just the set of seen
-- variables \Sigma \cap \Sigma' in his Algorithm M variant.
intersection :: Ord t => Assumption t -> Assumption t -> Assumption t
intersection (Assumption as) (Assumption bs) = Assumption $ S.toList $ go
  as
  bs
  mempty
 where
  go (a@(x, _) : xs) ys zs =
    let filtered = filter ((== x) . fst) ys in
        let zs' = if null filtered then zs else S.union zs $ S.fromList (a : filtered) in
            go xs ys zs'
  go [] _ zs = zs

-- | Runs 'intersection' on multiple 'Assumption's
intersectMany :: Ord t => [Assumption t] -> Assumption t
intersectMany as =
  Assumption . S.toList $ foldl' S.intersection S.empty (toSet <$> as)

-- | Remove all 'Assumption's with 'Name' in them
remove :: Assumption t -> Name -> Assumption t
remove (Assumption a) x = Assumption $ filter ((/= x) . fst) a

-- | Remove all 'Assumption's with any of the given 'Name's in them
removeMany :: Assumption t -> [Name] -> Assumption t
removeMany (Assumption a) xs =
  Assumption $ filter (\(x, _) -> x `notElem` xs) a

-- | Finds all @t@ in 'Assumption' paired with a given 'Name'
lookup :: Name -> Assumption t -> [t]
lookup x (Assumption a) = snd <$> filter ((== x) . fst) a
