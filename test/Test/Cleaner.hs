{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cleaner where

import qualified Control.Concurrent.STM           as STM
import qualified Control.Monad                    as Monad
import           Data.Text                        (Text)
import qualified Database.PostgreSQL.Simple       as PG
import qualified Database.PostgreSQL.Simple.Types as PG

import           DB

cleanLogs :: STM.TVar [String] -> IO ()
cleanLogs messages = do
  STM.atomically $ STM.writeTVar messages []

cleanAppState :: STM.TVar [String] -> (PG.Connection -> IO a) -> IO a
cleanAppState messages f = do
  cleanLogs messages
  cleanDBBefore f

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
