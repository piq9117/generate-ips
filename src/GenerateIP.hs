{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GenerateIP
  ( generateIpV4,
    fillQueue,
    drainQueue,
    newBuffer,
  )
where

import CLI (Command (..), GenerateIp (..))
import Conduit
  ( MonadUnliftIO,
    mapC,
    runConduitRes,
    sinkFile,
    (.|),
  )
import Control.Concurrent.STM.TBQueue
  ( TBQueue,
    newTBQueueIO,
    writeTBQueue,
  )
import Data.Conduit.Combinators (stdout)
import Data.Conduit.TQueue (sourceTBQueue)
import System.Random (randomRIO)
import Prelude hiding (stdout)

newBuffer :: Natural -> IO (TBQueue Text)
newBuffer size = newTBQueueIO size

fillQueue :: (MonadIO m) => TBQueue Text -> Int -> m ()
fillQueue queue count = do
  replicateM_ count $ do
    ipv4 <- generateIpV4
    atomically $ (writeTBQueue queue ipv4)

drainQueue :: (MonadIO m, MonadUnliftIO m) => TBQueue Text -> Command -> m ()
drainQueue queue (Generate input) = runConduitRes $
  case input.filepath of
    Nothing -> sourceIp .| stdout
    Just filepath -> sourceIp .| sinkFile filepath
  where
    sourceIp = sourceTBQueue queue .| mapC encodeIp
    encodeIp ip = encodeUtf8 $ ip <> "\n"

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
