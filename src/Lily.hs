{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Lily
  ( lily
  )
where

import           Control.Lens
import qualified Data.Graph                    as G
import           Control.Monad                  ( when )
import qualified Data.Map                      as M
import qualified Data.ByteString.Char8         as BS
import           Data.Foldable                  ( for_ )
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

lily :: FilePath -> IO ()
lily filepath = do
  idx <- createIndex
  tu  <- parseTranslationUnit idx filepath []

  let functions = translationUnitCursor tu ^.. allFunctions
  for_ functions $ \fn -> do
    putStrLn . showLoc . T.withoutKind $ fn
    let called = fn ^.. calledFunctions

    when (fn `elem` called) $ putStrLn "~ self - r e c u r s i v e ~"

    putStrLn $ unlines $ ("* " ++) . showLoc . T.withoutKind <$> called

showLoc cur = case fileLoc cur of
  Just Location {..} ->
    BS.unpack (cursorSpelling cur)
      <> " :: "
      <> maybe "<NoType>" (BS.unpack . typeSpelling) (cursorType cur)
      <> " ["
      <> show column
      <> ":"
      <> show line
      <> "]"
  Nothing ->
    BS.unpack (cursorSpelling cur)
      <> " :: "
      <> maybe "<NoType>" (BS.unpack . typeSpelling) (cursorType cur)
      <> " [no loc]"

fileLoc x = spellingLocation . rangeStart <$> cursorExtent x

