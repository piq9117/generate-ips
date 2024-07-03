{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import CLI (Command (..), GenerateIp (..), appParser)
import GenerateIP (newBuffer)
import Handler (handler)

main :: IO ()
main = do
  command@(Generate input) <- appParser
  buffer <- newBuffer (fromIntegral input.count)
  handler buffer command
