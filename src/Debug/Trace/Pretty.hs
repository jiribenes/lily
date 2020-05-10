module Debug.Trace.Pretty
  ( tracePretty
  , tracePrettyId
  , tracePrettyM
  )
where

import           Data.Text.Prettyprint.Doc      ( pretty
                                                , Pretty
                                                )
import           Debug.Trace

-- | Takes a 'Pretty' value, prints it and returns something else
tracePretty :: Pretty a => a -> b -> b
tracePretty x y = traceShow (pretty x) y

-- | Takes a 'Pretty' value, prints it and returns it
tracePrettyId :: Pretty a => a -> a
tracePrettyId x = tracePretty x x

-- | Takes a 'Pretty' value in a 'Applicative' context
-- and prints it
tracePrettyM :: (Pretty a, Applicative f) => a -> f ()
tracePrettyM x = traceShowM (pretty x)
