{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Weather.Api where

import           Servant
import           Servant.API.Flatten (Flat)

import           Server.Model
import           Weather.App

type WeatherApi =
  Flat (
    "api" :> "weather" :> Capture "location" String :> (
      Get '[JSON] String :<|>
      ReqBody '[JSON] String :> Put '[JSON] String
    )
  )

weatherHandler :: ServerT WeatherApi AppM
weatherHandler = getWeatherHandler :<|> setWeatherHandler
