{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Clang.Function
  ( isCanonical
  , functionHasBody
  , recursiveComponents
  , toSomeFunction
  , FunctionCursor
  , toFunction
  , FunctionTemplateCursor
  , toFunctionTemplate
  , SomeFunctionCursor(..)
  , someSpelling
  , someUSR
  , unwrapSomeFunction
  )
where

import           Control.Applicative            ( (<|>) )
import           Control.Lens
import           Data.ByteString.Char8          ( ByteString )
import           Data.Function                  ( on )
import qualified Data.Graph                    as G
import           Data.List                      ( groupBy
                                                , sortOn
                                                )
import           Data.Maybe                     ( fromJust )
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

type FunctionCursor = T.CursorK 'FunctionDecl
type FunctionTemplateCursor = T.CursorK 'FunctionTemplate

data SomeFunctionCursor = SomeFunction FunctionCursor
                        | SomeFunctionTemplate FunctionTemplateCursor
    deriving stock (Eq, Show)

makePrisms ''SomeFunctionCursor

unwrapSomeFunction :: SomeFunctionCursor -> Cursor
unwrapSomeFunction (SomeFunction         f ) = T.withoutKind f
unwrapSomeFunction (SomeFunctionTemplate ft) = T.withoutKind ft

someUSR :: SomeFunctionCursor -> ByteString
someUSR = cursorUSR . unwrapSomeFunction

someSpelling :: SomeFunctionCursor -> ByteString
someSpelling = cursorSpelling . unwrapSomeFunction

-- | Returns true if this cursor is actually canonical!
isCanonical :: Cursor -> Bool
isCanonical c = cursorCanonical c == c

functionHasBody :: SomeFunctionCursor -> Bool
functionHasBody =
  any (\child -> cursorKind child == CompoundStmt)
    . cursorChildren
    . unwrapSomeFunction

toFunction :: Cursor -> Maybe FunctionCursor
toFunction = T.matchKind @ 'FunctionDecl

toFunctionTemplate :: Cursor -> Maybe FunctionTemplateCursor
toFunctionTemplate = T.matchKind @ 'FunctionTemplate

toSomeFunction :: Cursor -> Maybe SomeFunctionCursor
toSomeFunction c =
  (SomeFunction <$> toFunction c)
    <|> (SomeFunctionTemplate <$> toFunctionTemplate c)

calledFunctions :: Fold Cursor SomeFunctionCursor
calledFunctions = referencedCalls . functionDecls
 where
  referencedCalls :: Fold Cursor Cursor
  referencedCalls =
    cursorDescendantsF
      . folding (T.matchKind @ 'CallExpr)
      -- . filtered ({-isFromMainFile . -} rangeStart . T.cursorExtent)
      . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)

  functionDecls :: Fold Cursor SomeFunctionCursor
  functionDecls = folding toSomeFunction

allFunctions :: Fold Cursor SomeFunctionCursor
allFunctions = cursorDescendantsF . folding toSomeFunction
   -- . filtered ({-isFromMainFile . -} rangeStart . T.cursorExtent)

getFunctionBody
  :: SomeFunctionCursor -> TranslationUnit -> Maybe SomeFunctionCursor
getFunctionBody def tu = translationUnitCursor tu ^? functionBodyF
 where
  functionBodyF :: Fold Cursor SomeFunctionCursor
  functionBodyF = allFunctions . filtered
    (\x ->
      (  cursorCanonical (unwrapSomeFunction x)
      == (unwrapSomeFunction def)
      && functionHasBody x
      )
    )

type FunctionGraphNode = (SomeFunctionCursor, ByteString, [ByteString])
type FunctionGraph = [FunctionGraphNode]

-- | TODO: We will always have at most one _real_ implementation, right?
normalize :: FunctionGraph -> FunctionGraph
normalize = fmap representGroup . groupByUSR
 where
  representGroup xs =
    ( fnCursor
    , cursorUSR $ unwrapSomeFunction fnCursor
    , xs ^.. traverse . _3 . traverse -- gather all `_3` in a single list
    )
   where
    fnCursor :: SomeFunctionCursor
    fnCursor = xs ^?! _head . _1 -- this is safe because when we get here in the actual program, 
                                       -- we already know that we have at least one cursor :)

  groupByUSR = groupBy ((==) `on` view _2) . sortOn (view _2)

recursiveComponents :: TranslationUnit -> [G.SCC SomeFunctionCursor]
recursiveComponents tu =
  translationUnitCursor tu
    ^.. allFunctions
    .   to intoGraphNode
    &   normalize
    &   G.stronglyConnComp
 where
  intoGraphNode :: SomeFunctionCursor -> FunctionGraphNode
  intoGraphNode fnDecl =
    ( fromJust $ getFunctionBody fnDecl tu
    , fnDecl & someUSR
    , (fnDecl & unwrapSomeFunction) ^.. calledFunctions . to someUSR
    )