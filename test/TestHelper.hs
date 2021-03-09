{-# LANGUAGE OverloadedStrings #-}

module TestHelper where

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

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action =
  either Exception.throwIO return =<< runClientM action clientEnv

withClient :: IO Application -> Hspec.SpecWith ClientEnv -> Hspec.SpecWith ()
withClient app innerSpec =
  Hspec.beforeAll (Client.newManager Client.defaultManagerSettings) $
    flip Hspec.aroundWith innerSpec $ \action httpManager ->
      testWithApplication app $ \port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action $ ClientEnv httpManager testBaseUrl Nothing defaultMakeClientRequest

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
