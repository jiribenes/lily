{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Name
  ( Name(Name)
  , nameFromBS
  , nameIsNull
  )
where

import           Data.String                    ( IsString )
import           Data.Text.Prettyprint.Doc      ( Pretty )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.ByteString.Char8          ( ByteString )
import           Data.Text.Encoding             ( decodeUtf8 )

-- | Represents an identifier, used for type constructors, type variables, variables, function names, etc.
newtype Name = Name { unName :: Text }
  deriving newtype (Eq, Ord, IsString, Pretty, Semigroup, Monoid)

instance Show Name where
  show = T.unpack . unName

-- | Helper function to create a 'Name' from a 'ByteString'.
-- This is useful because the clang-pure represents its identifiers as 'ByteString's. 
nameFromBS :: ByteString -> Name
nameFromBS = Name . decodeUtf8

-- | Checks if a 'Name' is null (an empty string)
nameIsNull :: Name -> Bool
nameIsNull = T.null . unName
