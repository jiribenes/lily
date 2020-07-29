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
  , ConstructorCursor
  , toConstructor
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
import           Data.List                      ( nub
                                                , groupBy
                                                , sortOn
                                                )
import           Data.Maybe                     ( fromJust )
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

type FunctionCursor = T.CursorK 'FunctionDecl
type FunctionTemplateCursor = T.CursorK 'FunctionTemplate
type ConstructorCursor = T.CursorK 'Constructor

-- | Represents some function 'Cursor'
data SomeFunctionCursor = SomeFunction FunctionCursor
                        | SomeFunctionTemplate FunctionTemplateCursor
                        | SomeConstructor ConstructorCursor
    deriving stock (Eq, Show)

makePrisms ''SomeFunctionCursor

-- | Unwraps 'SomeFunctionCursor' into a plain 'Cursor'
unwrapSomeFunction :: SomeFunctionCursor -> Cursor
unwrapSomeFunction (SomeFunction         f ) = T.withoutKind f
unwrapSomeFunction (SomeFunctionTemplate ft) = T.withoutKind ft
unwrapSomeFunction (SomeConstructor      c ) = T.withoutKind c

-- | Returns USR for a 'SomeFunctionCursor'
someUSR :: SomeFunctionCursor -> ByteString
someUSR = cursorUSR . unwrapSomeFunction

-- | Returns spelling for a 'SomeFunctionCursor'
someSpelling :: SomeFunctionCursor -> ByteString
someSpelling = cursorSpelling . unwrapSomeFunction

-- | Returns true if this cursor is actually canonical
isCanonical :: Cursor -> Bool
isCanonical c = cursorCanonical c == c

-- | Check if a function has body
functionHasBody :: SomeFunctionCursor -> Bool
functionHasBody =
  any (\child -> cursorKind child == CompoundStmt)
    . cursorChildren
    . unwrapSomeFunction

-- | Attempts to convert a 'Cursor' into a 'SomeFunctionCursor'
toSomeFunction :: Cursor -> Maybe SomeFunctionCursor
toSomeFunction c =
  (SomeFunction <$> toFunction c)
    <|> (SomeFunctionTemplate <$> toFunctionTemplate c)
    <|> (SomeConstructor <$> toConstructor c)

-- | Attempts to convert a 'Cursor' into a 'FunctionCursor'
toFunction :: Cursor -> Maybe FunctionCursor
toFunction = T.matchKind @ 'FunctionDecl

-- | Attempts to convert a 'Cursor' into a 'FunctionTemplateCursor'
toFunctionTemplate :: Cursor -> Maybe FunctionTemplateCursor
toFunctionTemplate = T.matchKind @ 'FunctionTemplate

-- | Attempts to convert a 'Cursor' into a 'ConstructorCursor'
toConstructor :: Cursor -> Maybe ConstructorCursor
toConstructor = T.matchKind @ 'Constructor

-- | Combines two 'Fold's
combineFolds :: Eq b => Fold a b -> Fold a b -> Fold a b
combineFolds f g = folding (\x -> nub $ (x ^.. f) <> (x ^.. g))

-- | Returns a 'Fold' representing all function cursors of functions called from under the given 'Cursor'
calledFunctions :: Fold Cursor SomeFunctionCursor
calledFunctions =
  cursorDescendantsF
    . combineFolds referencedCalls referencedCtorCalls
    . functionDecls
 where
  referencedCalls :: Fold Cursor Cursor
  referencedCalls =
    folding (T.matchKind @ 'DeclRefExpr)
      . filtered (isFromMainFile . rangeStart . T.cursorExtent)
      . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)
  -- this is in fact necessary and sufficient because we have to reference previously declared functions,
  -- but they can also be just references to a function pointer instead of a 'CallExpr'

  referencedCtorCalls :: Fold Cursor Cursor
  referencedCtorCalls =
    folding (T.matchKind @ 'CallExpr)
      . filtered (isFromMainFile . rangeStart . T.cursorExtent)
      . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)

  functionDecls :: Fold Cursor SomeFunctionCursor
  functionDecls = folding toSomeFunction

-- | Returns all functions under a cursor, including the ones just mentioned
allFunctions :: Fold Cursor SomeFunctionCursor
allFunctions =
  cursorDescendantsF
    . filtered (isFromMainFile . rangeStart . fromJust . cursorExtent)
    . folding toSomeFunction

-- | Returns a function body of a 'SomeFunctionCursor' if there is any
getFunctionBody
  :: SomeFunctionCursor -> TranslationUnit -> Maybe SomeFunctionCursor
getFunctionBody def tu = translationUnitCursor tu ^? functionBodyF
 where
  functionBodyF :: Fold Cursor SomeFunctionCursor
  functionBodyF = allFunctions . filtered
    (\x ->
      cursorCanonical (unwrapSomeFunction x)
        == unwrapSomeFunction def
        && functionHasBody x
    )

type FunctionGraphNode = (SomeFunctionCursor, ByteString, [ByteString])
type FunctionGraph = [FunctionGraphNode]

-- | Normalizes a function graph by selecting a representative for every function
normalize :: FunctionGraph -> FunctionGraph
normalize = fmap representGroup . groupByUSR
 where
  representGroup xs =
    ( fnCursor
    , fnCursor & unwrapSomeFunction & cursorUSR
    , xs ^.. traverse . _3 . traverse -- gather all `_3` in a single list
    )
   where
    fnCursor :: SomeFunctionCursor
    fnCursor = xs ^?! _head . _1 -- this is safe because when we get here in the actual program, 
                                 -- we already know that we have at least one cursor :)

  groupByUSR = groupBy ((==) `on` view _2) . sortOn (view _2)

-- | Returns the strongly connected components of a function graph
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
    ( getFunctionBody fnDecl tu & fromJust
    , fnDecl & someUSR
    , (fnDecl & unwrapSomeFunction) ^.. calledFunctions . to someUSR
    )
