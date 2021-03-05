{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Weather.Api where

import           Servant
import           Servant.API.Flatten (Flat)

import           Server.Model
import           Weather.App
import           Weather.Model

type WeatherApi =
  Flat (
    "api" :> "weather" :> Capture "location" String :> (
      Get '[JSON] Weather :<|>
      ReqBody '[JSON] String :> Put '[JSON] Weather
    )
  )

weatherHandler :: ServerT WeatherApi AppM
weatherHandler = getWeatherHandler :<|> setWeatherHandler
