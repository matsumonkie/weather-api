{-# LANGUAGE QuasiQuotes #-}

module City.Sql where

import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           City.Model
import           DB

selectCity :: String -> IO (Maybe City)
selectCity location = do
  withDBConnection $ \connection ->
    PG.query connection sqlQuery (PG.Only location) <&> Maybe.listToMaybe
  where
    sqlQuery =
      [sql|
          SELECT id, name
          FROM city
          WHERE name = ?
          LIMIT 1
          |]
