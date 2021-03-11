{-# LANGUAGE FlexibleContexts #-}

module Environment where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import           Prelude                hiding (log)

data Environment = Environment
    { envLog        :: String -> IO ()
    , envSendReport :: String -> IO ()
    }

log :: (Reader.MonadReader Environment m, IO.MonadIO m) => String -> m ()
log message = do
  environment <- Reader.ask
  IO.liftIO $ envLog environment message

sendReport :: (Reader.MonadReader Environment m, IO.MonadIO m) => String -> m ()
sendReport report = do
  environment <- Reader.ask
  IO.liftIO $ envSendReport environment report
