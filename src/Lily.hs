{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import           System.Exit                    ( exitFailure )

import           Clang
import           Clang.AST
import           Clang.Function
import qualified Clang.Struct                  as C
import           Core.Desugar                   ( desugarTopLevel )
import           Type.Infer                     ( inferTop
                                                , typeEnv
                                                )

lily :: FilePath -> IO ()
lily filepath = do
  tu <- createTranslationUnit filepath [] >>= \case
    Just x  -> pure x
    Nothing -> exitFailure

  printAST tu
  putStrLn "----------------------"
  let structs = C.structs tu
  print structs

  putStrLn "----------------------"
  let scc = recursiveComponents tu
  print scc

  putStrLn "----------------------"
  scc' <- case desugarTopLevel structs scc of
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
      putStrLn "Error happened during inference!"
      putStrLn "----------------------"
      putDoc $ pretty err
      putStrLn ""
      putStrLn "======================"
      pure mempty
    Right newEnv -> pure newEnv

  let prettyInferEnv = finalEnv ^. typeEnv . to M.toList & each %~ prettify

  putDoc $ PP.align (PP.vcat prettyInferEnv)
 where
  prettify (name, sch) = PP.fillBreak 10 (pretty name) <+> "::" <+> pretty sch
