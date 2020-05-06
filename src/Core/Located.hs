{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Located
  ( Location(..)
  , Located(..)
  )
where

import           Language.C.Clang.Location
import           Core.Syntax
import           Language.C.Clang.Cursor        ( cursorExtent
                                                , Cursor
                                                )
import           Data.Maybe                     ( fromJust )
import           Control.Lens                   ( to
                                                , (^.)
                                                , view
                                                )

newtype Range = Range { unRange :: (Location, Location) }
    deriving (Show, Eq)

class Located a where
    loc :: a -> Location

class Ranged a where
    range :: a -> Range

instance Ranged a => Located a where -- this instance is technically undecidable... but it shouldn't take GHC too much bother!
  loc = fst . unRange . range

instance Ranged SourceRange where
  range = Range . sourceRangeToLocation
   where
    sourceRangeToLocation =
      (,) <$> (spellingLocation . rangeStart) <*> (spellingLocation . rangeEnd)

instance {-# OVERLAPPABLE #-} HasCursor a => Ranged a where -- this isn't really overlappable, but GHC is mad at me...
  range a = range sr
   where
    sr :: SourceRange
    sr = a ^. cursorL . to (fromJust . cursorExtent)
