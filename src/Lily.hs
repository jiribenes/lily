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
import           Core.Syntax                    ( TopLevel )
import           Options
import           Type.Infer                     ( inferTop
                                                , typeEnv
                                                , InferState
                                                )

includes :: [String]
includes =
  ("-I" <>)
    <$> [ "/nix/store/52x4908vr922dhnxz1i5rfnrsq244vzc-gcc-9.3.0/lib/gcc/x86_64-unknown-linux-gnu/9.3.0/../../../../include/c++/9.3.0"
        , "/nix/store/52x4908vr922dhnxz1i5rfnrsq244vzc-gcc-9.3.0/lib/gcc/x86_64-unknown-linux-gnu/9.3.0/../../../../include/c++/9.3.0/x86_64-unknown-linux-gnu"
        , "/nix/store/52x4908vr922dhnxz1i5rfnrsq244vzc-gcc-9.3.0/lib/gcc/x86_64-unknown-linux-gnu/9.3.0/../../../../include/c++/9.3.0/backward"
        , "/nix/store/52x4908vr922dhnxz1i5rfnrsq244vzc-gcc-9.3.0/lib/gcc/x86_64-unknown-linux-gnu/9.3.0/include"
        , "/nix/store/52x4908vr922dhnxz1i5rfnrsq244vzc-gcc-9.3.0/include"
        , "/nix/store/52x4908vr922dhnxz1i5rfnrsq244vzc-gcc-9.3.0/lib/gcc/x86_64-unknown-linux-gnu/9.3.0/include-fixed"
        , "/nix/store/2m6n8flsmhvn19b9l3c622y6rzi81y5w-glibc-2.30-dev/include"
        ]

lily :: Options -> IO ()
lily opts = do
  (structs, sccs) <- parseAST (opts & optClangArguments .~ includes) -- JB hack

  toplevels       <- elaborate opts (structs, sccs)
  when (opts ^. optCommand == Elaborate) exitSuccess
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

elaborate
  :: Options
  -> ([C.StructCursor], [G.SCC C.SomeFunctionCursor])
  -> IO [TopLevel]
elaborate opts (structs, fnSccs) = case elaborateTopLevel structs fnSccs of
  Left err -> do
    printError Elaborate err
    exitFailure
  Right xs -> do
    when (opts ^. optCommand == Elaborate || opts ^. optVerbose) $ do
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
