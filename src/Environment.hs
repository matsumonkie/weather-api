{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Environment where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import           Data.Text              (Text)
import qualified Dhall
import           GHC.Word               (Word16)
import           Prelude                hiding (log)

data Config = Config
    { cfgSecretToken :: String
    , cfgDBPort      :: Word16
    , cfgDBName      :: String
    , cfgDBUser      :: String
    , cfgDBPassword  :: String
    }
    deriving Show

instance Dhall.FromDhall Config where
  autoWith _ = Dhall.record $
    Config
      <$> Dhall.field "secretToken" Dhall.string
      <*> Dhall.field "dbPort" Dhall.word16
      <*> Dhall.field "dbName" Dhall.string
      <*> Dhall.field "dbUser" Dhall.string
      <*> Dhall.field "dbPassword" Dhall.string

loadConfig :: Text -> IO Config
loadConfig file =
  Dhall.input Dhall.auto file

data Environment = Environment
    { envLog        :: String -> IO ()
    , envSendReport :: String -> IO ()
    , envConfig     :: Config
    }

log :: (Reader.MonadReader Environment m, IO.MonadIO m) => String -> m ()
log message = do
  environment <- Reader.ask
  IO.liftIO $ envLog environment message

sendReport :: (Reader.MonadReader Environment m, IO.MonadIO m) => String -> m ()
sendReport report = do
  environment <- Reader.ask
  IO.liftIO $ envSendReport environment report
