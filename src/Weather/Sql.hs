{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Weather.Sql where

import qualified Control.Monad.IO.Class           as IO
import qualified Control.Monad.Reader             as Reader
import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Int                          (Int64)

import           City.Model
import           DB
import           Environment
import           Weather.Model

selectCityWeather
  :: ( Reader.MonadReader Environment m
     , IO.MonadIO m
     )
  => City -> m (Maybe Weather)
selectCityWeather city = do
  withDBConnection $ \connection ->
    PG.query connection sqlQuery (PG.Only (cityId city)) <&> Maybe.listToMaybe
  where
    sqlQuery =
      [sql|
          SELECT id, name
          FROM weather
          WHERE id = (
            SELECT weather_id
            FROM city_weather
            WHERE city_id = ?
          )
          |]

selectWeather
  :: ( Reader.MonadReader Environment m
     , IO.MonadIO m
     )
  => String -> m (Maybe Weather)
selectWeather location = do
  withDBConnection $ \connection ->
    PG.query connection sqlQuery (PG.Only location) <&> Maybe.listToMaybe
  where
    sqlQuery =
      [sql|
          SELECT id, name
          FROM weather
          WHERE name = ?
          LIMIT 1
          |]

setCityWeather
  :: ( Reader.MonadReader Environment m
     , IO.MonadIO m
     )
  => City -> Weather -> m Int64
setCityWeather city weather = do
  withDBConnection $ \connection ->
    PG.execute connection sqlQuery ( cityId city
                                   , weatherId weather
                                   , weatherId weather
                                   )
  where
    sqlQuery =
      [sql|
          INSERT INTO city_weather(id, city_id, weather_id)
          VALUES(gen_random_uuid(), ?, ?)
          ON CONFLICT (city_id) DO
            UPDATE SET weather_id = ?
          |]
