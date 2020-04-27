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

lily :: FilePath -> IO ()
lily filepath = do
  tu <- createTranslationUnit filepath []
  let scc = recursiveComponents tu
  print scc

  let initialEnv = mempty
  finalEnv <- foldlM go initialEnv scc
  print finalEnv
  pure ()
 where
  go :: InferEnv -> SCC FunctionCursor -> IO InferEnv 
  go env (AcyclicSCC func) = case inferTop env [("name", func)] of 
    Left err -> do
      putStrLn $ "Error happened! " <> show err
      pure env
    Right newEnv ->
      pure newEnv