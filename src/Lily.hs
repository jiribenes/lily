{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lily
  ( lily
  )
where

import           Control.Lens
import qualified Data.Map                      as M
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(pretty)
                                                )
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc.Render.Text
                                                ( putDoc )

import           Clang
import           Clang.AST
import           Clang.Function
import           Core.Desugar                   ( desugarTopLevel )
import           Type.Infer                     ( inferTop
                                                , typeEnv
                                                )

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
  finalEnv <- case inferTop initialEnv scc' of
    Left err -> do
      putStrLn "======================"
      putStrLn "Error happened!"
      putStrLn "----------------------"
      putDoc $ pretty err
      putStrLn ""
      putStrLn "======================"
      pure mempty
    Right newEnv -> pure newEnv

  let prettyInferEnv = finalEnv ^. typeEnv . to M.toList & each %~ prettify

  putDoc $ "let" <+> PP.align (PP.vcat prettyInferEnv)
  where prettify (name, sch) = PP.fill 5 (pretty name) <+> "::" <+> pretty sch
