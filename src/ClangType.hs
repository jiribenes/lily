module ClangType where

import           Type    
import           Language.C.Clang.Type (typePointeeType,  typeCanonicalType, typeSpelling )
import qualified Language.C.Clang.Type as Clang 
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

type ClangType = Clang.Type

fromClangType :: ClangType -> Type
fromClangType clangType = case Clang.typeKind canonicalClangType of
  Clang.Invalid -> error "invalid clang type"
  Clang.Unexposed -> error "unexposed type"
  Clang.Void -> typeUnit
  Clang.Bool -> typeBool
  Clang.Int  -> typeInt
  Clang.Pointer -> let inner = typePointeeType canonicalClangType in typePtrOf (fromClangType $ fromJust inner)
  other -> error $ unlines ["internal error: Encountered unknown Clang type (no conversion available!)", "\t\tSpelling:\t" <> (BS.unpack $ typeSpelling clangType), "\t\tKind:\t'" <> show other <> "'"]

 where
  canonicalClangType = typeCanonicalType clangType  