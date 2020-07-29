{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Located
  ( Location(..)
  , prettyLocation
  , Located(..)
  )
where

import           Control.Lens                   ( (^.)
                                                , to
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.Maybe                     ( fromJust )
import qualified Data.Text.Prettyprint.Doc     as PP
import           Language.C.Clang.Cursor        ( cursorExtent )
import           Language.C.Clang.File
import           Language.C.Clang.Location
import qualified Language.C.Clang.Cursor.Typed as T

import           Core.Syntax
import           Clang

-- | Range is just a pair of 'Location's
newtype Range = Range { unRange :: (Location, Location) }
    deriving stock (Show, Eq)

-- | A function to get a pretty representation of a 'Location'
prettyLocation :: Location -> PP.Doc ann
prettyLocation (Location f l c _) = PP.concatWith
  (PP.surround ":")
  [PP.pretty $ BS.unpack $ fileName f, PP.pretty l, PP.pretty c]

-- | A type class for locatable things (things that returns a 'Location')
class Located a where
    loc :: a -> Location

-- | A type class for things with range (things that return a 'Range')
class Ranged a where
    range :: a -> Range

-- | Every 'Ranged' is convertible to a 'Located' by just taking the beginning location of the range.
instance Ranged a => Located a where -- this instance is technically undecidable... but it shouldn't take GHC too much bother!
  loc = fst . unRange . range

-- | Clang's 'SourceRange' is 'Ranged'
instance Ranged SourceRange where
  range = Range . sourceRangeToLocation
   where
    sourceRangeToLocation =
      (,) <$> (spellingLocation . rangeStart) <*> (spellingLocation . rangeEnd)

-- | Every thing with a 'Cursor' is 'Ranged'
instance {-# OVERLAPPABLE #-} HasCursor a => Ranged a where -- this isn't really overlappable, but GHC is mad at me...
  range a = range sr
   where
    sr :: SourceRange
    sr = a ^. cursorL . to (fromJust . cursorExtent)

-- | Every AST node with extent is 'Ranged'
instance T.HasExtent k => Ranged (T.CursorK k) where
  range c = range sr
   where
    sr :: SourceRange
    sr = T.cursorExtent c

-- | Every function is 'Ranged'
instance Ranged SomeFunctionCursor where
  range a = range sr
   where
    sr :: SourceRange
    sr = fromJust $ cursorExtent $ unwrapSomeFunction a
