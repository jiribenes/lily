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
import           Core.Syntax                    ( TopLevel'(TLLet)
                                                , nameL
                                                , TopLevel
                                                )
import           Infer                          ( inferTop
                                                , InferEnv
                                                , typeEnv
                                                )
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
  scc' <- case desugarTopLevel scc of
    Left err -> do
      putStrLn "Desugaring failed!"
      putStrLn $ "Error: " <> show err
      pure []
    Right xs -> do
      putDoc $ PP.align $ PP.vcat $ pretty <$> xs
      putStrLn ""
      pure xs

  putStrLn "----------------------"
  let initialEnv = mempty
  finalEnv <- foldlM go initialEnv scc'

  let finalInferEnv = finalEnv ^. typeEnv . to M.toList

  putDoc $ "let" <+> PP.align (PP.vcat $ prettify <$> finalInferEnv)
  pure ()
 where
  prettify (name, typ) = PP.fill 5 (pretty name) <+> "::" <+> pretty typ

  go :: InferEnv -> TopLevel -> IO InferEnv
  go env l@(TLLet nonRecursiveLet) =
    case inferTop env [(nonRecursiveLet ^. nameL, l)] of
      Left err -> do
        putStrLn "======================"
        putStrLn "Error happened!"
        putStrLn "----------------------"
        putDoc $ pretty err
        putStrLn ""
        putStrLn "======================"
        pure env
      Right newEnv -> pure newEnv
  go env _ = do
    traceM $ "TODO Ignoring a cyclic dependency for now!"
    pure env -- ignore
