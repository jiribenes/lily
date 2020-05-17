module Clang.AST
  ( printAST
  )
where

import qualified Data.ByteString.Char8         as BS
import           Data.Foldable                  ( traverse_ )
import           Data.Maybe                     ( isJust )
import           Language.C.Clang               ( TranslationUnit )
import           Language.C.Clang.Cursor        ( cursorUSR
                                                , cursorReferenced
                                                , Cursor
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
    let hasRef = if isJust $ cursorReferenced c then "[has ref]" else ""
    let usr =
          let usr' = BS.unpack (cursorUSR c)
          in  if null usr' then "" else " {" <> BS.unpack (cursorUSR c) <> "}"

    putStrLn $ indent i kind <> canon <> usr <> if null spelling
      then ""
      else ", " <> spelling <> if null hasBody
        then ""
        else ", " <> hasBody <> if null hasRef then "" else ", " <> hasRef
    traverse_ (go (i + 4)) $ cursorChildren c

  indent :: Int -> String -> String
  indent i s = replicate i ' ' <> s
