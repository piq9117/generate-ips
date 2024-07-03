{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( handler,
  )
where

import CLI (Command (..), GenerateIp (..))
import Conduit (withRunInIO)
import Control.Concurrent.Async (concurrently_, replicateConcurrently_)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Exception (catch)
import GHC.IO.Exception (BlockedIndefinitelyOnSTM)
import GenerateIP (drainQueue, fillQueue)

handler :: TBQueue Text -> Command -> IO ()
handler queue command@(Generate input) =
  concurrently_
    (fillQueue queue input.count)
    ( replicateConcurrently_ 8 $
        withRunInIO $ \runInIO ->
          runInIO $ drainQueue queue command `catch` (\(_e :: BlockedIndefinitelyOnSTM) -> pure ())
    )
