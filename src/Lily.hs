{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lily
  ( lily
  )
where

import           Clang
import           Infer ( inferTop, InferEnv )
import Data.Graph (SCC(..))
import Data.Foldable (foldlM)
import Data.Text.Encoding (decodeUtf8)
import Data.Function ((&))

lily :: FilePath -> IO ()
lily filepath = do
  tu <- createTranslationUnit filepath []
  printAST tu

  let scc = recursiveComponents tu
  print scc

  let initialEnv = mempty
  finalEnv <- foldlM go initialEnv scc
  print finalEnv
  pure ()
 where
  go :: InferEnv -> SCC SomeFunctionCursor -> IO InferEnv 
  go env (AcyclicSCC func) = case inferTop env [(func & someSpelling & decodeUtf8, func)] of 
    Left err -> do
      putStrLn $ "Error happened! " <> show err
      pure env
    Right newEnv ->
      pure newEnv