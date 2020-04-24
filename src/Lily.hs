{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Lily
  ( lily
  )
where

import           Control.Lens
import           Data.ByteString.Char8          ( ByteString )
import           Data.Function                  ( on )
import qualified Data.Graph                    as G
import           Data.List                      ( groupBy
                                                , sortOn
                                                )
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

type FunctionCursor = T.CursorK 'FunctionDecl

toFunction :: Cursor -> Maybe FunctionCursor
toFunction = T.matchKind @ 'FunctionDecl

calledFunctions :: Fold FunctionCursor FunctionCursor
calledFunctions = referencedCalls . functionDecls
 where
  referencedCalls :: Fold FunctionCursor Cursor
  referencedCalls =
    T.cursorDescendantsF
      . folding (T.matchKind @ 'CallExpr)
      -- . filtered ({-isFromMainFile . -} rangeStart . T.cursorExtent)
      . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)
  functionDecls :: Fold Cursor FunctionCursor
  functionDecls = folding toFunction

allFunctions :: Fold Cursor FunctionCursor
allFunctions = cursorDescendantsF . folding toFunction
   -- . filtered ({-isFromMainFile . -} rangeStart . T.cursorExtent)

type FunctionGraphNode = (FunctionCursor, ByteString, [ByteString])
type FunctionGraph = [FunctionGraphNode]

normalize :: FunctionGraph -> FunctionGraph
normalize = fmap representGroup . groupByUSR
 where
  representGroup xs =
    ( fnCursor
    , cursorUSR . T.withoutKind $ fnCursor
    , xs ^.. traverse . _3 . traverse -- gather all `_3` in a single list
    )
    where fnCursor = xs ^?! _head . _1 -- this is safe because when we get here in the actual program, 
                                       -- we already know that we have at least one cursor :)

  groupByUSR = groupBy ((==) `on` view _2) . sortOn (view _2)

lily :: FilePath -> IO ()
lily filepath = do
  idx <- createIndexWithOptions [DisplayDiagnostics]
  tu  <- parseTranslationUnit idx filepath []
  let scc = recursiveComponents tu
  print scc
  pure ()

recursiveComponents :: TranslationUnit -> [G.SCC FunctionCursor]
recursiveComponents tu =
  translationUnitCursor tu
    ^.. allFunctions
    .   to intoGraphNode
    &   normalize
    &   G.stronglyConnComp
 where
  intoGraphNode :: FunctionCursor -> FunctionGraphNode
  intoGraphNode fnDecl =
    ( fnDecl
    , typedCursorUSR fnDecl
    , fnDecl ^.. calledFunctions . to typedCursorUSR
    )
    where typedCursorUSR = cursorUSR . T.withoutKind
