{-# LANGUAGE ScopedTypeVariables #-}

module Test.Helper ( module Test.Client
                   , module Test.Cleaner
                   , try
                   , readMessages
                   , errorsWithStatus
                   ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception      as Exception
import           Network.HTTP.Types     (Status)
import           Servant.Client

import           Test.Cleaner
import           Test.Client

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action =
  either Exception.throwIO return =<< runClientM action clientEnv

readMessages :: STM.TVar [String] -> IO [String]
readMessages messages =
  STM.readTVarIO messages

errorsWithStatus :: Status -> ClientError -> Bool
errorsWithStatus status servantError =
  case servantError of
    FailureResponse _ response -> responseStatusCode response == status
    _                          -> False
