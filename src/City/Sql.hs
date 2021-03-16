{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module City.Sql where

import qualified Control.Monad.IO.Class           as IO
import qualified Control.Monad.Reader             as Reader
import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           City.Model
import           DB
import           Environment

selectCity
  :: ( Reader.MonadReader Environment m
     , IO.MonadIO m
     )
  => String
  -> m (Maybe City)
selectCity location = do
  withDBConnection $ \connection ->
    IO.liftIO $ PG.query connection sqlQuery (PG.Only location) <&> Maybe.listToMaybe
  where
    sqlQuery =
      [sql|
          SELECT id, name
          FROM city
          WHERE name = ?
          LIMIT 1
          |]
