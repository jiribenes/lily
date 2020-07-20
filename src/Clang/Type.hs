module Clang.Type
  ( ClangType
  , fromClangType
  )
where

import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( (<&>) )
import           Debug.Trace                    (traceShowId,  traceShow )
import           GHC.Stack                      ( HasCallStack )
import           Language.C.Clang.Type          ( typeElementType
                                                , typeArgTypes
                                                , typeCanonicalType
                                                , typePointeeType
                                                , typeResultType
                                                , typeSpelling
                                                )
import qualified Language.C.Clang.Type         as ClangType

import           Type.Type
import           Name                           ( nameFromBS )
import           Control.Monad                  ( unless )
import Data.Maybe (fromJust)

data Ref = LRef | RRef | UnspecifiedRef

type ClangType = ClangType.Type

-- Note: This function is technically partial because Clang
-- can have 'ClangType.Type.Invalid' type kind
-- which indicates an unrecoverable error in Clang itself.
-- We'll just error in that case since what else is to do there?
fromClangType :: HasCallStack => ClangType -> Maybe Type
fromClangType clangType = case traceShowId $ ClangType.typeKind canonicalClangType of
  ClangType.Invalid   -> error "internal error: invalid Clang type!"
  ClangType.Unexposed -> traceShow
    (  "Found type: "
    <> BS.unpack (typeSpelling clangType)
    <> ", canonically:"
    <> BS.unpack (typeSpelling canonicalClangType)
    <> ", but returning Nothing for now! TODO"
    )
    Nothing -- TODO: this generally means that the type is a bit involved and you should just use a new type constructor with the given spelling!
  ClangType.Void   -> pure typeUnit
  ClangType.Bool   -> pure typeBool
  ClangType.Int    -> pure typeInt
  ClangType.UInt   -> pure typeUInt
  ClangType.Char_S -> pure typeChar
  ClangType.Pointer -> do
    let canonicalPointee = typeCanonicalType $ fromJust $ typePointeeType canonicalClangType
    case ClangType.typeKind canonicalPointee of
      -- special case for function pointers
      ClangType.FunctionProto -> fromClangType canonicalPointee
      _ -> fromClangType canonicalPointee <&> typePtrOf
  ClangType.Record -> do
    let name = nameFromBS $ typeSpelling canonicalClangType
    let tc   = TC name StarKind -- TODO: get kind from clang
    pure (TCon tc)
  ClangType.LValueReference ->
    typePointeeType canonicalClangType >>= fromClangType <&> MutRef
  ClangType.FunctionProto -> do
    clangArgTypes <- typeArgTypes canonicalClangType
    resultType    <- fromClangType =<< typeResultType canonicalClangType

    argTypes      <- traverse getArgumentType clangArgTypes

    pure $ foldr (\(ty, ref) acc -> (refToArrow ref) `TAp` ty `TAp` acc) resultType argTypes
  ClangType.ConstantArray ->
    typeElementType canonicalClangType >>= fromClangType <&> typeListOf
  other -> traceShow
    (unlines
      [ "internal error: Encountered unknown Clang type (no conversion available!)"
      , "\t\tName:\t\t\t" <> BS.unpack (typeSpelling clangType)
      , "\t\tCanonical type name:\t"
        <> BS.unpack (typeSpelling canonicalClangType)
      , "\t\tKind:\t\t\t'" <> show other <> "'"
      ]
    )
    Nothing
  where canonicalClangType = typeCanonicalType clangType

refToArrow :: Ref -> Type
refToArrow RRef = typeLinArrow
refToArrow LRef = typeUnArrow
refToArrow UnspecifiedRef = typeLinArrow

getArgumentType :: HasCallStack => ClangType -> Maybe (Type, Ref)
getArgumentType clangType = case ClangType.typeKind canonicalClangType of
  ClangType.LValueReference -> do
    let maybePointee = typePointeeType canonicalClangType
    ty <- fromClangType =<< maybePointee
    pure (ty, LRef)
  ClangType.RValueReference -> do
    let maybePointee = typePointeeType canonicalClangType
    ty <- fromClangType =<< maybePointee
    pure (ty, LRef) 
  other -> do
    ty <- fromClangType clangType
    pure (ty, UnspecifiedRef)
  where canonicalClangType = typeCanonicalType clangType