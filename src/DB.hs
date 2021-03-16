{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module DB (withDBConnection) where

import qualified Control.Monad.IO.Class     as IO
import qualified Control.Monad.Reader       as Reader
import           Data.Functor               ((<&>))
import qualified Database.PostgreSQL.Simple as PG

import           Environment

withDBConnection
  :: ( Reader.MonadReader Environment m
     , IO.MonadIO m
     )
  => (PG.Connection -> IO a) -> m a
withDBConnection f = do
  connection <- getDBConnection
  res <- IO.liftIO $ f connection
  closeDBConnection connection
  return res

getDBConnection :: (Reader.MonadReader Environment m, IO.MonadIO m) => m PG.Connection
getDBConnection = do
  Config{..} <- Reader.ask <&> envConfig
  IO.liftIO $ PG.connect PG.defaultConnectInfo
    { PG.connectDatabase = cfgDBName
    , PG.connectUser = cfgDBUser
    , PG.connectPort = cfgDBPort
    , PG.connectPassword = cfgDBPassword
    }

closeDBConnection :: (IO.MonadIO m) => PG.Connection -> m ()
closeDBConnection connection =
  IO.liftIO $ PG.close connection
