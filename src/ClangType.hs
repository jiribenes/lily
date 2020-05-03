module ClangType (fromClangType) where

import           Type    
import           Language.C.Clang.Type (typePointeeType,  typeCanonicalType, typeSpelling )
import qualified Language.C.Clang.Type as Clang 
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<&>))
import Debug.Trace (traceShow)

type ClangType = Clang.Type

fromClangType :: ClangType -> Maybe Type
fromClangType clangType = case Clang.typeKind canonicalClangType of
  Clang.Invalid -> error "invalid clang type"
  Clang.Unexposed -> traceShow ("Found type: " <> (BS.unpack $ typeSpelling clangType) <> ", canonically:" <> (BS.unpack $ typeSpelling canonicalClangType) <> ", but returning Nothing for now! TODO") Nothing -- TODO: this generally means that the type is a bit involved and you should just use a new type constructor with the given spelling!
  Clang.Void -> pure typeUnit
  Clang.Bool -> pure typeBool
  Clang.Int  -> pure typeInt
  Clang.Pointer -> typePointeeType canonicalClangType >>= fromClangType <&> typePtrOf
  other -> error $ unlines ["internal error: Encountered unknown Clang type (no conversion available!)", "\t\tSpelling:\t" <> (BS.unpack $ typeSpelling clangType), "\t\tKind:\t'" <> show other <> "'"]

 where
  canonicalClangType = typeCanonicalType clangType  