module Main where

import           Server.App (run)

main :: IO ()
main = do
  run 8888
