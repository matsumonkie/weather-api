{-# LANGUAGE QuasiQuotes #-}

module Weather.Sql where

import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Int                          (Int64)

import           City.Model
import           DB
import           Weather.Model

selectCityWeather :: City -> IO (Maybe Weather)
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

selectWeather :: String -> IO (Maybe Weather)
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

setCityWeather :: City -> Weather -> IO Int64
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
