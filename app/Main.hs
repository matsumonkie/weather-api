{-# LANGUAGE TypeApplications #-}

module Main where

import           System.Environment

import           Server.App         (run)

main :: IO ()
main = do
  [port] <- getArgs
  run $ read @Int port
