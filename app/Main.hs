module Main
  ( main
  )
where

import           Lily                           ( lily )
import           Options                        ( parseOptions )

main :: IO ()
main = parseOptions >>= lily