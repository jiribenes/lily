module Main
  ( main
  )
where

import           Lily                           ( lily )
import           System.Environment             ( getArgs )
import           System.Directory               ( doesFileExist )
import           Control.Monad                  ( unless )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      exists <- doesFileExist file
      unless exists $ putStrLn "Error: invalid file!"
      lily file
    _ -> do
      putStrLn "Error: invalid arguments!"
      putStrLn "Expected: lily <filepath>"
