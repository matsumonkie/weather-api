module Test.Client where

import qualified Control.Concurrent.STM   as STM
import           Environment
import qualified Network.HTTP.Client      as Client
import           Network.Wai.Handler.Warp (testWithApplication)
import           Servant
import           Servant.Client
import qualified Test.Hspec               as Hspec

import           Server.App               (mkApp)

data TestEnv = TestEnv
    { testClientEnv :: ClientEnv
    , testMessages  :: STM.TVar [String]
    }

withClient :: Hspec.SpecWith TestEnv -> Hspec.SpecWith ()
withClient innerSpec = do
  (messages, application) <- Hspec.runIO buildApp
  Hspec.beforeAll (Client.newManager Client.defaultManagerSettings) $
    flip Hspec.aroundWith innerSpec $ \action httpManager ->
      testWithApplication (return application) $ \port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        let testEnv = TestEnv { testClientEnv = ClientEnv httpManager testBaseUrl Nothing defaultMakeClientRequest
                              , testMessages = messages
                              }
        action testEnv
   where
     buildApp :: IO (STM.TVar [String], Application)
     buildApp = do
       messages <- STM.newTVarIO []
       let logFunc newMsg = STM.atomically $ STM.modifyTVar messages (\state -> state ++ [newMsg])
       app <- mkApp $ Environment { envLog = logFunc
                                  , envSendReport = \_ -> return ()
                                  }
       return (messages, app)
