{-# LANGUAGE ScopedTypeVariables #-}

module Weather.AppSpec where

import qualified Network.HTTP.Types as HTTP
import           Servant
import           Servant.Client     (ClientM)
import qualified Servant.Client     as Servant
import           Test.Hspec

import           DBUtil
import           Server.App         (mkApp)
import           TestHelper
import           Weather.Api
import           Weather.Model

getWeatherHandler :: String -> ClientM Weather
setWeatherHandler :: String -> String -> ClientM Weather
getWeatherHandler :<|> setWeatherHandler =
  Servant.client (Proxy :: Proxy WeatherApi)

spec :: Spec
spec = do
  withClient mkApp $ do
    describe "getting the weather" $ do
      it "should fail when the location is unknown" $ \clientEnv ->
        try clientEnv (getWeatherHandler "unknown location") `shouldThrow` errorsWithStatus HTTP.notFound404

      it "should return the weather when the location exists" $ \clientEnv ->
        cleanDBBefore $ \connection -> do
          let newCity = "marseille"
          _ <- insertCityWeather connection newCity "sunny"
          weather <- try clientEnv (getWeatherHandler newCity)
          weatherName weather `shouldBe` "sunny"
