module ClangType where

import           Type    
import           Language.C.Clang.Type ( typeCanonicalType, typeSpelling )
import qualified Language.C.Clang.Type as Clang 

type ClangType = Clang.Type

fromClangType :: ClangType -> Type
fromClangType clangType = case Clang.typeKind canonicalClangType of
  Clang.Invalid -> error "invalid clang type"
  Clang.Unexposed -> error "unexposed type"
  Clang.Void -> typeUnit
  Clang.Bool -> typeBool
  Clang.Int  -> typeInt
  _ -> error "internal error: unknown Clang type (no conversion available!)"

 where
  canonicalClangType = typeCanonicalType clangType    