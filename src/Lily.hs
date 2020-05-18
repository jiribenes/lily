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

import qualified Clang                         as C
import           Core.Desugar                   ( desugarTopLevel )
import           Type.Infer                     ( inferTop
                                                , typeEnv
                                                )

lily :: FilePath -> IO ()
lily filepath = do
  tu <- C.createTranslationUnit filepath [] >>= \case
    Just x  -> pure x
    Nothing -> exitFailure

  C.printAST tu
  let structs = C.structs tu
  let scc     = C.recursiveComponents tu

  putStrLn "----------------------"
  scc' <- case desugarTopLevel structs scc of
    Left err -> do
      putStrLn "======================"
      putStrLn "Error happened during desugaring!"
      putStrLn "----------------------"
      putDoc $ pretty err
      putStrLn ""
      putStrLn "======================"
      exitFailure
    Right xs -> do
      putDoc $ PP.align $ PP.vcat $ pretty <$> xs
      putStrLn ""
      pure xs

  putStrLn "----------------------"
  finalEnv <- case inferTop scc' of
    Left err -> do
      putStrLn "======================"
      putStrLn "Error happened during inference!"
      putStrLn "----------------------"
      putDoc $ pretty err
      putStrLn ""
      putStrLn "======================"
      exitFailure
    Right newEnv -> pure newEnv

  let prettyInferEnv = finalEnv ^. typeEnv . to M.toList & each %~ prettify

  putDoc $ PP.align (PP.vcat prettyInferEnv)
 where
  prettify (name, sch) = PP.fillBreak 10 (pretty name) <+> "::" <+> pretty sch
