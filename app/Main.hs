module Main where

import CLI (appParser)

main :: IO ()
main = do
  command <- appParser
  putStrLn $ show command
