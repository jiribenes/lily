{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Clang
  ( createTranslationUnit
  , recursiveComponents
  , FunctionCursor
  , FunctionTemplateCursor
  , SomeFunctionCursor(..)
  , someSpelling
  , someUSR
  , unwrapSomeFunction
  , printAST
  )
where

import           Control.Arrow                  ( (|||) )
import           Control.Lens
import           Control.Applicative
import           Data.Functor.Adjunction        ( uncozipL )
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BS
import           Data.Foldable                  ( traverse_ )
import           Data.Function                  ( on )
import qualified Data.Graph                    as G
import           Data.List                      ( groupBy
                                                , sortOn
                                                )
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

import Debug.Trace

createTranslationUnit :: FilePath -> [String] -> IO TranslationUnit
createTranslationUnit filepath clangOptions = do
  idx <- createIndexWithOptions [DisplayDiagnostics]
  parseTranslationUnit idx filepath clangOptions

type FunctionCursor = T.CursorK 'FunctionDecl
type FunctionTemplateCursor = T.CursorK 'FunctionTemplate

data SomeFunctionCursor = SomeFunction FunctionCursor
                        | SomeFunctionTemplate FunctionTemplateCursor
    deriving stock (Eq, Show)

makePrisms ''SomeFunctionCursor

unwrapSomeFunction :: SomeFunctionCursor -> Cursor 
unwrapSomeFunction (SomeFunction f) = T.withoutKind f
unwrapSomeFunction (SomeFunctionTemplate ft) = T.withoutKind ft

someUSR :: SomeFunctionCursor -> ByteString
someUSR = cursorUSR . unwrapSomeFunction

someSpelling :: SomeFunctionCursor -> ByteString
someSpelling = cursorSpelling . unwrapSomeFunction

toFunction :: Cursor -> Maybe FunctionCursor
toFunction = T.matchKind @ 'FunctionDecl

toFunctionTemplate :: Cursor -> Maybe FunctionTemplateCursor
toFunctionTemplate = T.matchKind @ 'FunctionTemplate

toSomeFunction :: Cursor -> Maybe SomeFunctionCursor
toSomeFunction c = (SomeFunction <$> toFunction c) <|> (SomeFunctionTemplate <$> toFunctionTemplate c)  

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

type FunctionGraphNode = (SomeFunctionCursor, ByteString, [ByteString])
type FunctionGraph = [FunctionGraphNode]

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
    ( fnDecl
    , fnDecl & someUSR
    , (fnDecl & unwrapSomeFunction) ^.. calledFunctions . to someUSR
    )

printAST :: TranslationUnit -> IO ()
printAST tu = go 0 $ translationUnitCursor tu
    where
        go :: Int -> Cursor  -> IO ()
        go i c = do
            let kind = show $ cursorKind c
            let spelling = BS.unpack $ cursorSpelling c
            putStrLn $ (indent i kind) <> ", " <> spelling
            traverse_ (go (i + 4)) $ cursorChildren c

        indent :: Int -> String -> String
        indent i s = (replicate i ' ') <> s
