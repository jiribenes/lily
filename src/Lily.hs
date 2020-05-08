{-# LANGUAGE TypeApplications #-}
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
import           Core.Desugar                   ( desugarTopLevel )
import           Infer                          ( inferTop
                                                , InferEnv
                                                )
import           Type                           ( nameFromBS )
import           Data.Graph                     ( SCC(..) )
import           Data.Foldable                  ( for_
                                                , foldlM
                                                )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Debug.Trace                    ( traceM )

lily :: FilePath -> IO ()
lily filepath = do
  tu <- createTranslationUnit filepath []
  printAST tu


  putStrLn "----------------------"
  let scc = recursiveComponents tu
  print scc

  putStrLn "----------------------"
  case
      desugarTopLevel
      $   fmap (fromJust . toFunction . unwrapSomeFunction) -- TODO: this is unsafe! will crash on encountering FunctionTemplate!
      <$> scc
    of
      Right x   -> for_ x print
      Left  err -> putStrLn $ "Desugaring failed! Error: " <> show err

  putStrLn "----------------------"
  let initialEnv = mempty
  finalEnv <- foldlM go initialEnv scc
  print finalEnv
  pure ()
 where
  go :: InferEnv -> SCC SomeFunctionCursor -> IO InferEnv
  go env (AcyclicSCC func) =
    case inferTop env [(func & someSpelling & nameFromBS, func)] of
      Left err -> do
        putStrLn $ "Error happened! " <> show err
        pure env
      Right newEnv -> pure newEnv
  go env (CyclicSCC funcs) = do
    traceM
      $  "TODO Ignoring a cyclic dependency for now!"
      <> (unwords $ show <$> funcs)
    pure env -- ignore
