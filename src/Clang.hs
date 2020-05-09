module Clang
  ( 
  createTranslationUnit
  )
where

-- TODO: Reexport clang-pure things
-- Ideally, we'll only use this module and it's submodules
-- and not clang-pure directly! 

import           Language.C.Clang               ( parseTranslationUnit
                                                , ClangIndexOption
                                                  ( DisplayDiagnostics
                                                  )
                                                , createIndexWithOptions
                                                , TranslationUnit
                                                )


createTranslationUnit :: FilePath -> [String] -> IO TranslationUnit
createTranslationUnit filepath clangOptions = do
  idx <- createIndexWithOptions [DisplayDiagnostics]
  parseTranslationUnit idx filepath clangOptions
