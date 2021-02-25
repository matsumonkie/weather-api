{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Model where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import           Servant

newtype AppM a =
  AppM { unAppM :: Except.ExceptT ServerError IO a }
  deriving ( Except.MonadError ServerError
           , Functor
           , Applicative
           , Monad
           , IO.MonadIO
           )

appMToHandler :: AppM a -> Handler a
appMToHandler r = do
  eitherErrorOrResult <- IO.liftIO $ Except.runExceptT . unAppM $ r
  case eitherErrorOrResult of
    Left err     -> throwError err
    Right result -> return result
