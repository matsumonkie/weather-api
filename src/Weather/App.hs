{-# LANGUAGE FlexibleContexts #-}

module Weather.App where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import           Prelude                hiding (log)
import qualified Servant

import           City.Model
import           City.Sql
import           Environment
import           Weather.Model
import           Weather.Sql

getWeatherHandler
  :: ( IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     , Reader.MonadReader Environment m
     )
  => String -> m Weather
getWeatherHandler location = do
  mCity <- selectCity location
  case mCity of
    Nothing -> do
      sendReport $ "missing location: " <> show location
      Servant.throwError Servant.err404
    Just city -> do
      mWeather <- selectCityWeather city
      case mWeather of
        Nothing -> do
          log $ "uh?! somethings went wrong, we couldn't get the weather for city " <> cityName city
          Servant.throwError Servant.err500
        Just weather -> return weather

setWeatherHandler
  :: ( IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     , Reader.MonadReader Environment m
     )
  => String -> String -> m Weather
setWeatherHandler location newWeather = do
  mCity <- selectCity location
  mWeather <- selectWeather newWeather
  case (mCity, mWeather) of
    (Just city, Just weather) -> do
      _ <- setCityWeather city weather
      return weather

    _ -> Servant.throwError Servant.err404
