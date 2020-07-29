{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Clang.Struct
  ( StructCursor
  , structsF
  , structs
  )
where

import           Control.Lens
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

type StructCursor = T.CursorK 'StructDecl

-- | A Fold from any cursor to all structs under it
structsF :: Fold Cursor StructCursor
structsF =
  cursorDescendantsF
    . folding (T.matchKind @ 'StructDecl)
    . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)
    . folding (T.matchKind @ 'StructDecl)

-- | Returns a list of all structs in the translation unit
structs :: TranslationUnit -> [StructCursor]
structs tu = translationUnitCursor tu ^.. structsF
