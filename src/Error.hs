module Error ( note ) where

import Control.Monad.Except (throwError, MonadError)

-- | Unwraps a `Maybe` value, throws an error if the value was `Nothing`,
--    otherwise returns the unwrapped value.
note :: MonadError e m => e -> Maybe a -> m a
note err = maybe (throwError err) pure 