{-# LANGUAGE FlexibleContexts #-}

module Weather.App where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Servant

getWeatherHandler :: ( IO.MonadIO m, Except.MonadError Servant.ServerError m ) => String -> m String
getWeatherHandler location = do
  case location of
    "Paris"     -> return "rainy"
    "Nantes"    -> return "cloudy"
    "Marseille" -> return "sunny"
    _           -> Servant.throwError Servant.err404

setWeatherHandler :: ( IO.MonadIO m, Except.MonadError Servant.ServerError m ) => String -> String -> m String
setWeatherHandler location newWeather =
  case location of
    "Paris"     -> return newWeather
    "Nantes"    -> return newWeather
    "Marseille" -> return newWeather
    _           -> Servant.throwError Servant.err404
