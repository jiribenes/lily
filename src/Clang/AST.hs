module Clang.AST
  ( printAST
  )
where


import qualified Data.ByteString.Char8         as BS
import           Data.Foldable                  ( traverse_ )
import           Language.C.Clang               ( TranslationUnit )
import           Language.C.Clang.Cursor        ( Cursor
                                                , cursorChildren
                                                , cursorKind
                                                , cursorSpelling
                                                , translationUnitCursor
                                                )

import           Clang.Function


printAST :: TranslationUnit -> IO ()
printAST tu = go 0 $ translationUnitCursor tu
 where
  go :: Int -> Cursor -> IO ()
  go i c = do
    let kind     = show $ cursorKind c
    let spelling = BS.unpack $ cursorSpelling c
    let canon    = if isCanonical c then " [canon]" else ""
    let hasBody =
          maybe "" (\x -> if functionHasBody x then "[has body]" else "")
            $ toSomeFunction c

    putStrLn $ indent i kind <> canon <> if null spelling
      then ""
      else ", " <> spelling <> if null hasBody then "" else ", " <> hasBody
    traverse_ (go (i + 4)) $ cursorChildren c

  indent :: Int -> String -> String
  indent i s = replicate i ' ' <> s
