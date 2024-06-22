{-# LANGUAGE OverloadedStrings #-}

module GenerateIP
  ( generateIpV4,
  )
where

import System.Random (randomRIO)

generateIpV4 :: (MonadIO m) => m Text
generateIpV4 = do
  ipElement1 <- generateIpElement
  ipElement2 <- generateIpElement
  ipElement3 <- generateIpElement
  ipElement4 <- generateIpElement
  pure $
    show ipElement1
      <> "."
      <> show ipElement2
      <> "."
      <> show ipElement3
      <> "."
      <> show ipElement4

generateIpElement :: (MonadIO m) => m Int
generateIpElement = randomRIO (0, 999)
