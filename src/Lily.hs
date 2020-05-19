{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lily
  ( lily
  )
where

import           Control.Lens
import           Control.Monad                  ( unless
                                                , when
                                                )
import qualified Data.Map                      as M
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(pretty)
                                                )
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc.Render.Text
                                                ( putDoc )
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import qualified Data.Graph                    as G

import qualified Clang                         as C
import           Core.Desugar                   ( desugarTopLevel )
import           Core.Syntax                    ( TopLevel )
import           Options
import           Type.Infer                     ( inferTop
                                                , typeEnv
                                                , InferState
                                                )

lily :: Options -> IO ()
lily opts = do
  (structs, sccs) <- parseAST opts

  toplevels       <- desugar opts (structs, sccs)
  when (opts ^. optCommand == Desugar) exitSuccess
  putStrLn "----------------------"

  finalEnv <- infer opts toplevels
  when (opts ^. optCommand == Infer) exitSuccess
  putStrLn "----------------------"

  when (opts ^. optCommand == Lint) $ do
    putStrLn "Error: Command 'lint' is not supported yet!"
    exitFailure

parseAST :: Options -> IO ([C.StructCursor], [G.SCC C.SomeFunctionCursor])
parseAST opts = do
  let sourceFile = opts ^. optSource
  sourceFileExists <- doesFileExist sourceFile

  unless sourceFileExists $ putStrLn "Error: invalid source file!"

  tu <- C.createTranslationUnit sourceFile (opts ^. optClangArguments) >>= \case
    Just x  -> pure x
    Nothing -> exitFailure

  when (opts ^. optVerbose) $ C.printAST tu
  let structs = C.structs tu
  let scc     = C.recursiveComponents tu

  pure (structs, scc)

desugar
  :: Options
  -> ([C.StructCursor], [G.SCC C.SomeFunctionCursor])
  -> IO [TopLevel]
desugar opts (structs, fnSccs) = case desugarTopLevel structs fnSccs of
  Left err -> do
    printError Desugar err
    exitFailure
  Right xs -> do
    when (opts ^. optCommand == Desugar || opts ^. optVerbose) $ do
      putDoc $ PP.align $ PP.vcat $ pretty <$> xs
      putStrLn ""
    pure xs

infer :: Options -> [TopLevel] -> IO InferState
infer opts toplevels = case inferTop toplevels of
  Left err -> do
    printError Infer err
    exitFailure
  Right xs -> do
    when (opts ^. optCommand == Infer || opts ^. optVerbose) $ do
      putDoc $ PP.align $ PP.vcat $ prettyInferState xs
      putStrLn ""
    pure xs

-- -------------------------

printError :: Pretty a => Command -> a -> IO ()
printError cmd err = do
  putStrLn "======================"
  putStrLn $ "Error happened during " <> show cmd <> "!"
  putStrLn "----------------------"
  putDoc $ pretty err
  putStrLn ""
  putStrLn "======================"

prettyInferState finalEnv =
  finalEnv ^. typeEnv . to M.toList & each %~ prettify
 where
  prettify (name, sch) = PP.fillBreak 10 (pretty name) <+> "::" <+> pretty sch
