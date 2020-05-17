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
import           Clang.Function

newtype Range = Range { unRange :: (Location, Location) }
    deriving stock (Show, Eq)

prettyLocation :: Location -> PP.Doc ann
prettyLocation (Location f l c _) = PP.concatWith
  (PP.surround ":")
  [PP.pretty $ BS.unpack $ fileName f, PP.pretty l, PP.pretty c]

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

instance T.HasExtent k => Ranged (T.CursorK k) where
  range c = range sr
   where
    sr :: SourceRange
    sr = T.cursorExtent c

instance Ranged SomeFunctionCursor where
  range a = range sr
   where
    sr :: SourceRange
    sr = fromJust $ cursorExtent $ unwrapSomeFunction a
