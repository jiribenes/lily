{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Lily
  ( lily
  )
where

import           Control.Lens
import           Control.Monad                  ( when )
import qualified Data.ByteString.Char8         as BS
import           Data.ByteString.Char8          ( ByteString )
import           Data.Foldable                  ( for_ )
import           Data.Function                  ( on )
import qualified Data.Graph                    as G
import qualified Data.Map                      as M
import qualified Data.Set                      as S
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
      . filtered (isFromMainFile . rangeStart . T.cursorExtent)
      . folding (cursorReferenced . T.withoutKind)
  functionDecls :: Fold Cursor FunctionCursor
  functionDecls = folding toFunction

allFunctions :: Fold Cursor FunctionCursor
allFunctions = cursorDescendantsF . folding toFunction . filtered
  (isFromMainFile . rangeStart . T.cursorExtent)

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
    where fnCursor = xs ^?! _head . _1

  groupByUSR = groupBy ((==) `on` view _2) . sortOn (view _2)


lily :: FilePath -> IO ()
lily filepath = do
  idx <- createIndex
  tu  <- parseTranslationUnit idx filepath []
  let functions = translationUnitCursor tu ^.. allFunctions
  let graph     = functions <&> intoGraphNode

  let scc = graph & normalize & G.stronglyConnComp
  pure ()
 where
  intoGraphNode :: FunctionCursor -> FunctionGraphNode
  intoGraphNode fnDecl =
    ( fnDecl
    , typedCursorUSR fnDecl
    , fnDecl ^.. calledFunctions . to typedCursorUSR
    )
    where typedCursorUSR = cursorUSR . T.withoutKind
