{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestHelper where

import qualified Control.Concurrent.STM           as STM
import qualified Control.Exception                as Exception
import qualified Control.Monad                    as Monad
import           Data.Text                        (Text)
import qualified Database.PostgreSQL.Simple       as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Network.HTTP.Client              as Client
import           Network.HTTP.Types               (Status)
import           Network.Wai.Handler.Warp         (testWithApplication)
import           Servant
import           Servant.Client
import qualified Test.Hspec                       as Hspec

import           DB
import           Environment
import           Server.App                       (mkApp)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action =
  either Exception.throwIO return =<< runClientM action clientEnv

readMessages :: STM.TVar [String] -> IO [String]
readMessages messages =
  STM.readTVarIO messages

cleanLogs :: STM.TVar [String] -> IO ()
cleanLogs messages = do
  STM.atomically $ STM.writeTVar messages []

cleanAppState :: STM.TVar [String] -> (PG.Connection -> IO a) -> IO a
cleanAppState messages f = do
  cleanLogs messages
  cleanDBBefore f

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

errorsWithStatus :: Status -> ClientError -> Bool
errorsWithStatus status servantError =
  case servantError of
    FailureResponse _ response -> responseStatusCode response == status
    _                          -> False

cleanDBBefore :: (PG.Connection -> IO a) -> IO a
cleanDBBefore f = do
  withDBConnection $ \connection -> do
    listTables connection >>= mapM_ (truncateTable connection)
    f connection
  where
    listTables :: PG.Connection -> IO [Text]
    listTables connection =
      map PG.fromOnly `fmap` PG.query_ connection sqlQuery
      where
        sqlQuery = mconcat [ " SELECT c.relname FROM pg_catalog.pg_class c"
                           , " LEFT JOIN pg_catalog.pg_namespace n"
                           , " ON c.relnamespace = n.oid"
                           , " WHERE c.relkind IN ('r', '')"
                           , " AND n.nspname <> 'pg_catalog'"
                           , " AND n.nspname <> 'information_schema'"
                           , " AND n.nspname !~ '^pg_toast'"
                           , " AND pg_catalog.pg_table_is_visible(c.oid)"
                           ]

    truncateTable :: PG.Connection -> Text -> IO ()
    truncateTable connection =
      Monad.void . PG.execute connection sqlQuery . PG.Only . PG.Identifier
      where
        sqlQuery = "TRUNCATE ? CASCADE"
