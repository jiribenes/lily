{-# LANGUAGE BangPatterns #-}
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
import           Core.Elaboration               ( elaborateTopLevel )
import           Core.Syntax                    ( Program )
import           Options
import           Type.Infer                     ( HasInferState
                                                , inferProgram
                                                , typeEnv
                                                , InferState
                                                )
import Lint ( lintProgram, Suggestion(..) )


lily :: Options -> IO ()
lily opts = do
  (structs, sccs) <- parseAST opts

  toplevels       <- elaborate opts (structs, sccs)
  when (opts ^. optCommand == Elaborate) exitSuccess

  finalEnv <- infer opts toplevels
  when (opts ^. optCommand == Infer) exitSuccess

  suggestions <- lint finalEnv toplevels
  when (opts ^. optCommand == Lint) exitSuccess

  putStrLn "Internal error: Invalid command - You added a new command to Options.hs and forgot to use it in Lily.hs"
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

elaborate
  :: Options
  -> ([C.StructCursor], [G.SCC C.SomeFunctionCursor])
  -> IO Program
elaborate opts (structs, fnSccs) = case elaborateTopLevel structs fnSccs of
  Left err -> do
    printError Elaborate err
    exitFailure
  Right xs -> do
    when (opts ^. optCommand == Elaborate || opts ^. optVerbose) $ do
      putDoc $ PP.align $ PP.vcat $ pretty <$> xs
      putStrLn ""
    pure xs

infer :: Options -> Program -> IO InferState
infer opts toplevels = case inferProgram toplevels of
  Left err -> do
    printError Infer err
    exitFailure
  Right xs -> do
    when (opts ^. optCommand == Infer || opts ^. optVerbose) $ do
      putDoc $ PP.align $ PP.vcat $ prettyInferState xs
      putStrLn ""
    pure xs

lint :: InferState -> Program -> IO [Suggestion]
lint is toplevels = do
  let suggestions = lintProgram is toplevels
  putDoc $ PP.align $ PP.vcat $ pretty <$> suggestions
  putStrLn ""
  pure $ suggestions

-- -------------------------

printError :: Pretty a => Command -> a -> IO ()
printError cmd err = do
  putStrLn "======================"
  putStrLn $ "Error happened during " <> show cmd <> "!"
  putStrLn "----------------------"
  putDoc $ pretty err
  putStrLn ""
  putStrLn "======================"

prettyInferState :: HasInferState s => s -> [PP.Doc ann]
prettyInferState finalEnv =
  finalEnv ^. typeEnv . to M.toList & each %~ prettify
 where
  prettify (name, sch) = PP.fillBreak 10 (pretty name) <+> "::" <+> pretty sch
