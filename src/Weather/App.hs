{-# LANGUAGE FlexibleContexts #-}

module Weather.App where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Servant

import           City.Sql
import           Weather.Model
import           Weather.Sql

getWeatherHandler :: ( IO.MonadIO m, Except.MonadError Servant.ServerError m ) => String -> m Weather
getWeatherHandler location = do
  mCity <- IO.liftIO $ selectCity location
  case mCity of
    Nothing -> Servant.throwError Servant.err404
    Just city -> do
      mWeather <- IO.liftIO $ selectCityWeather city
      case mWeather of
        Nothing      -> Servant.throwError Servant.err404
        Just weather -> return weather

setWeatherHandler :: ( IO.MonadIO m, Except.MonadError Servant.ServerError m ) => String -> String -> m Weather
setWeatherHandler location newWeather = do
  mCity <- IO.liftIO $ selectCity location
  mWeather <- IO.liftIO $ selectWeather newWeather
  case (mCity, mWeather) of
    (Just city, Just weather) -> do
      _ <- IO.liftIO $ setCityWeather city weather
      return weather

    _ -> Servant.throwError Servant.err404
