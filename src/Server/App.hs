{-# LANGUAGE DataKinds #-}

module Server.App(run, mkApp) where

import qualified Control.Concurrent       as Concurrent
import qualified Network.Wai.Handler.Warp as Warp
import qualified Say
import           Servant

import           Environment
import           Server.Model
import           Weather.Api

run :: Int -> IO ()
run port = do
  putStrLn $ "running api on port: " <> show port
  let environment = Environment { envLog = Say.sayString
                                , envSendReport = \_ -> Concurrent.threadDelay 2000
                                }
  app <- mkApp environment
  Warp.run port app

mkApp :: Environment -> IO Application
mkApp environment = do
  let webApiProxy = Proxy :: Proxy WeatherApi
  return $ serveWithContext webApiProxy EmptyContext $ hoistServerWithContext webApiProxy (Proxy :: Proxy '[]) (appMToHandler environment) weatherHandler
