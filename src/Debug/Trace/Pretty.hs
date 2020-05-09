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

tracePretty :: Pretty a => a -> b -> b
tracePretty x y = traceShow (pretty x) y

tracePrettyId :: Pretty a => a -> a
tracePrettyId x = tracePretty x x

tracePrettyM :: (Pretty a, Applicative f) => a -> f ()
tracePrettyM x = traceShowM (pretty x)
