{-# LANGUAGE TypeApplications #-}

module Clang
  ( module X
  , createTranslationUnit
  )
where

import           Language.C.Clang               ( parseTranslationUnit
                                                , ClangIndexOption
                                                  ( DisplayDiagnostics
                                                  )
                                                , createIndexWithOptions
                                                , TranslationUnit
                                                , ClangError
                                                , hasDiagnostics
                                                )

import           Control.Exception              ( try
                                                , displayException
                                                )

-- Reexport of all Clang submodules
import           Clang.AST                     as X
import           Clang.Function                as X
import           Clang.MemberParser            as X
import           Clang.Struct                  as X
import           Clang.Type                    as X

-- | Entry point to interaction with Clang. Takes a 'FilePath' and a list of arguments
-- and possibly returns the root of Clang AST for the file.
createTranslationUnit :: FilePath -> [String] -> IO (Maybe TranslationUnit)
createTranslationUnit filepath clangOptions = do
  idx <- createIndexWithOptions [DisplayDiagnostics]
  e   <- try @ClangError $ parseTranslationUnit idx filepath clangOptions
  case e of
    Left err -> do
      putStrLn "========================================="
      putStrLn "Clang Internal Error occured!"
      putStrLn $ displayException err
      pure Nothing
    Right tu -> do
      if hasDiagnostics tu
        then do
          putStrLn "========================================="
          putStrLn "Clang found some errors is your code."
          putStrLn "Lily will not work unless you solve them."
          pure Nothing
        else pure $ Just tu


