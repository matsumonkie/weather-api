{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Weather.AppSpec where

import qualified Network.HTTP.Types as HTTP
import           Servant
import           Servant.Client     (ClientM)
import qualified Servant.Client     as Servant
import           Test.Hspec

import           DBUtil
import           TestHelper
import           Weather.Api
import           Weather.Model

getWeatherHandler :: String -> ClientM Weather
setWeatherHandler :: String -> String -> ClientM Weather
getWeatherHandler :<|> setWeatherHandler =
  Servant.client (Proxy :: Proxy WeatherApi)

spec :: Spec
spec = do
  withClient $ do
    describe "getting the weather" $ do
      it "should fail when the location is unknown" $ \TestEnv{..} ->
        try testClientEnv (getWeatherHandler "unknown location") `shouldThrow` errorsWithStatus HTTP.notFound404

      it "should return the weather when the location exists" $ \TestEnv{..} ->
        cleanAppState testMessages $ \connection -> do
          let newCity = "marseille"
          _ <- insertCityWeather connection newCity "sunny"
          weather <- try testClientEnv (getWeatherHandler newCity)
          weatherName weather `shouldBe` "sunny"

      it "should logs a message if the city exists but doesn't have a weather" $ \TestEnv{..} ->
        cleanAppState testMessages $ \connection -> do
          let newCity = "marseille"
          _ <- insertCity connection newCity
          try testClientEnv (getWeatherHandler newCity) `shouldThrow` errorsWithStatus HTTP.internalServerError500
          readMessages testMessages `shouldReturn` [ "uh?! somethings went wrong, we couldn't get the weather for city marseille" ]
