{-# LANGUAGE OverloadedStrings #-}

module Test.Client where

import qualified Control.Concurrent.STM   as STM
import qualified Network.HTTP.Client      as Client
import           Network.Wai.Handler.Warp (testWithApplication)
import           Servant
import           Servant.Client
import qualified Test.Hspec               as Hspec

import           Environment
import           Server.App               (mkApp)

data TestEnv = TestEnv
    { testClientEnv   :: ClientEnv
    , testMessages    :: STM.TVar [String]
    , testEnvironment :: Environment
    }

withClient :: Hspec.SpecWith TestEnv -> Hspec.SpecWith ()
withClient innerSpec = do
  (environment, messages, application) <- Hspec.runIO buildApp
  Hspec.beforeAll (Client.newManager Client.defaultManagerSettings) $
    flip Hspec.aroundWith innerSpec $ \action httpManager ->
      testWithApplication (return application) $ \port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        let testEnv = TestEnv { testClientEnv = ClientEnv httpManager testBaseUrl Nothing defaultMakeClientRequest
                              , testEnvironment = environment
                              , testMessages = messages
                              }
        action testEnv
   where
     buildApp :: IO (Environment, STM.TVar [String], Application)
     buildApp = do
       messages <- STM.newTVarIO []
       let logFunc newMsg = STM.atomically $ STM.modifyTVar messages (\state -> state ++ [newMsg])
       config <- loadConfig "./test.dhall"
       let environment = Environment { envLog = logFunc
                                     , envSendReport = \_ -> return ()
                                     , envConfig = config
                                     }
       app <- mkApp environment
       return (environment, messages, app)
