{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Lily
  ( lily
  )
where

import           Control.Lens
import qualified Data.Graph                    as G
import qualified Data.Map                      as M
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

type FunctionCursor = T.CursorK 'FunctionDecl

calledFunctions :: Fold FunctionCursor FunctionCursor
calledFunctions = referencedCalls . functionDecls
 where
  referencedCalls :: Fold FunctionCursor Cursor
  referencedCalls =
    T.cursorDescendantsF
      . folding (T.matchKind @ 'CallExpr)
      . filtered (isFromMainFile . rangeStart . T.cursorExtent)
      . folding (cursorReferenced . T.withoutKind)

  functionDecls :: Fold Cursor FunctionCursor
  functionDecls = folding (T.matchKind @ 'FunctionDecl)

lily :: String
lily = "Hello world, I'm ~lily~!"
