{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Name (Name(Name), nameFromBS) where

import Data.String (IsString)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (decodeUtf8)

newtype Name = Name { unName :: Text }
  deriving newtype (Eq, Ord, IsString, Pretty)

instance Show Name where
  show = T.unpack . unName

nameFromBS :: ByteString -> Name
nameFromBS = Name . decodeUtf8