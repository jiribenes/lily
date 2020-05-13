{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Clang.Struct where


import           Control.Lens
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

type StructCursor = T.CursorK 'StructDecl

structsF :: Fold Cursor StructCursor
structsF =
  cursorDescendantsF
    . folding (T.matchKind @ 'StructDecl)
    . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)
    . folding (T.matchKind @ 'StructDecl)

structs :: TranslationUnit -> [StructCursor]
structs tu = translationUnitCursor tu ^.. structsF
