module Clang.Type
  ( ClangType
  , fromClangType
  )
where

import qualified Data.ByteString.Char8         as BS
import           Data.Functor                   ( (<&>) )
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
import           Data.Maybe                     ( fromJust )

type ClangType = ClangType.Type

-- | Takes a 'ClangType' and returns a possible 'Type' as known by Lily
--
-- Note: This function is technically partial because Clang
-- can have 'ClangType.Type.Invalid' type kind
-- which indicates an unrecoverable error in Clang itself.
-- We'll just error in that case since what else is to do there?
fromClangType :: HasCallStack => ClangType -> Maybe Type
fromClangType clangType =
  case ClangType.typeKind canonicalClangType of
    ClangType.Invalid   -> error "internal error: invalid Clang type!"
    ClangType.Unexposed -> error
      (  "internal error: Found type: "
      <> BS.unpack (typeSpelling clangType)
      <> ", canonically:"
      <> BS.unpack (typeSpelling canonicalClangType)
      )
    ClangType.Void    -> pure typeUnit
    ClangType.Bool    -> pure typeBool
    ClangType.Int     -> pure typeInt
    ClangType.UInt    -> pure typeUInt
    ClangType.Char_S  -> pure typeChar
    ClangType.Pointer -> do
      let canonicalPointee = typeCanonicalType $ fromJust $ typePointeeType canonicalClangType
      case ClangType.typeKind canonicalPointee of
        -- special case for function pointers
        ClangType.FunctionProto -> fromClangType canonicalPointee
        _                       -> fromClangType canonicalPointee <&> typePtrOf
    ClangType.Record -> do
      let name = nameFromBS $ typeSpelling canonicalClangType
      let tc   = TC name StarKind -- we presume that all types from Clang are of kind StarKind, thus ignoring templates
      pure (TCon tc)
    ClangType.LValueReference -> -- ignore LRef for type inference, see 'getArgumentType' for proper handling during linting
      typePointeeType canonicalClangType >>= fromClangType
    ClangType.RValueReference -> -- ignore RRef for type inference, see 'getArgumentType' for proper handling during linting
      typePointeeType canonicalClangType >>= fromClangType
    ClangType.FunctionProto -> do
      clangArgTypes <- typeArgTypes canonicalClangType
      resultType    <- fromClangType =<< typeResultType canonicalClangType

      argTypes      <- traverse getArgumentType clangArgTypes

      pure $ foldr UnArrow resultType argTypes
    ClangType.ConstantArray ->
      typeElementType canonicalClangType >>= fromClangType <&> typeListOf
    other -> error
      (unlines
        [ "internal error: Encountered unknown Clang type (no conversion available!)"
        , "\t\tName:\t\t\t" <> BS.unpack (typeSpelling clangType)
        , "\t\tCanonical type name:\t"
          <> BS.unpack (typeSpelling canonicalClangType)
        , "\t\tKind:\t\t\t'" <> show other <> "'"
        ]
      )
  where canonicalClangType = typeCanonicalType clangType

-- | Gets a Lily 'Type' from Clang's 'ClangType'.
-- Does NOT ignore LRef and RRef.
getArgumentType :: HasCallStack => ClangType -> Maybe Type
getArgumentType clangType = case ClangType.typeKind canonicalClangType of
  ClangType.LValueReference -> do
    let maybePointee = typePointeeType canonicalClangType
    ty <- fromClangType =<< maybePointee
    pure (LRef ty)
  ClangType.RValueReference -> do
    let maybePointee = typePointeeType canonicalClangType
    ty <- fromClangType =<< maybePointee
    pure (RRef ty)
  _ -> do
    ty <- fromClangType clangType
    pure ty
  where canonicalClangType = typeCanonicalType clangType
