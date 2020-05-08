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
                                                , typeEnv
                                                )
import           Type                           ( nameFromBS )
import           Data.Graph                     ( SCC(..) )
import           Data.Foldable                  ( foldlM )
import           Debug.Trace                    ( traceM )
import           Data.Text.Prettyprint.Doc.Render.Text
                                                ( putDoc )
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(pretty)
                                                )
import           Control.Lens
import qualified Data.Map                      as M
import qualified Data.Text.Prettyprint.Doc     as PP

lily :: FilePath -> IO ()
lily filepath = do
  tu <- createTranslationUnit filepath []
  printAST tu

  putStrLn "----------------------"
  let scc = recursiveComponents tu
  print scc

  putStrLn "----------------------"
  case desugarTopLevel scc of
    Right xs  -> putDoc $ PP.align $ PP.vcat $ pretty <$> xs
    Left  err -> putStrLn $ "Desugaring failed! Error: " <> show err

  putStrLn "----------------------"
  let initialEnv = mempty
  finalEnv <- foldlM go initialEnv scc

  let finalInferEnv = finalEnv ^. typeEnv . to M.toList

  putDoc $ "let" <+> PP.align (PP.vcat $ prettify <$> finalInferEnv)
 where
  prettify (name, typ) = PP.fill 5 (pretty name) <+> "::" <+> pretty typ

  go :: InferEnv -> SCC SomeFunctionCursor -> IO InferEnv
  go env (AcyclicSCC func) =
    case inferTop env [(func & someSpelling & nameFromBS, func)] of
      Left err -> do
        putStrLn $ "Error happened! " <> show (pretty err)
        pure env
      Right newEnv -> pure newEnv
  go env (CyclicSCC funcs) = do
    traceM $ "TODO Ignoring a cyclic dependency for now!" <> unwords
      (show <$> funcs)
    pure env -- ignore
