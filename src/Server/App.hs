{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}

module Server.App(run, mkApp) where

import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import           Server.Model
import           Weather.Api

run :: Int -> IO ()
run port = do
  putStrLn $ "running api on port: " <> show port
  app <- mkApp
  Warp.run port app

mkApp :: IO Application
mkApp = do
  let webApiProxy = Proxy :: Proxy WeatherApi
  return $ serveWithContext webApiProxy EmptyContext $ hoistServerWithContext webApiProxy (Proxy :: Proxy '[]) appMToHandler weatherHandler
