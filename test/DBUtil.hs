{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE QuasiQuotes    #-}

module DBUtil where

import           Data.UUID                        (UUID)
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Int                          (Int64)

insertCityWeather :: PG.Connection -> String -> String -> IO Int64
insertCityWeather connection cityName weatherName = do
  PG.execute connection sqlQuery (weatherName, cityName)
  where
    sqlQuery =
      [sql|
          WITH weatherId AS (
            INSERT INTO weather (id, name)
            VALUES (gen_random_uuid(), ?)
            RETURNING id
          ), cityId AS (
            INSERT INTO city (id, name)
            VALUES (gen_random_uuid(), ?)
            RETURNING id
          )
          INSERT INTO city_weather (id, city_id, weather_id)
          VALUES (gen_random_uuid(), (select id from cityId), (select id from weatherId))
          |]

insertCity :: PG.Connection -> String -> IO UUID
insertCity connection cityName = do
  [ PG.Only id ] <- PG.query connection sqlQuery (PG.Only cityName)
  return id
  where
    sqlQuery =
      [sql|
          INSERT INTO city (id, name)
          VALUES (gen_random_uuid(), ?)
          RETURNING id
          |]
