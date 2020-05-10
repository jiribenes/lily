module Clang.Type
  ( ClangType
  , fromClangType
  )
where

import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( (<&>) )
import           Debug.Trace                    ( traceShow )
import           Language.C.Clang.Type          ( typeCanonicalType
                                                , typePointeeType
                                                , typeSpelling
                                                )
import qualified Language.C.Clang.Type         as ClangType

import           Type.Type

type ClangType = ClangType.Type

fromClangType :: ClangType -> Maybe Type
fromClangType clangType = case ClangType.typeKind canonicalClangType of
  ClangType.Invalid   -> error "invalid clang type"
  ClangType.Unexposed -> traceShow
    (  "Found type: "
    <> BS.unpack (typeSpelling clangType)
    <> ", canonically:"
    <> BS.unpack (typeSpelling canonicalClangType)
    <> ", but returning Nothing for now! TODO"
    )
    Nothing -- TODO: this generally means that the type is a bit involved and you should just use a new type constructor with the given spelling!
  ClangType.Void -> pure typeUnit
  ClangType.Bool -> pure typeBool
  ClangType.Int  -> pure typeInt
  ClangType.UInt -> pure typeUInt
  ClangType.Pointer ->
    typePointeeType canonicalClangType >>= fromClangType <&> typePtrOf
  other -> error $ unlines
    [ "internal error: Encountered unknown Clang type (no conversion available!)"
    , "\t\tName:\t\t\t" <> BS.unpack (typeSpelling clangType)
    , "\t\tCanonical type name:\t"
      <> BS.unpack (typeSpelling canonicalClangType)
    , "\t\tKind:\t\t\t'" <> show other <> "'"
    ]
  where canonicalClangType = typeCanonicalType clangType
