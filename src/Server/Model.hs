{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Model where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import           Servant

import           Environment

newtype AppM a =
  AppM { unAppM :: Except.ExceptT ServerError (Reader.ReaderT Environment IO) a }
  deriving ( Except.MonadError ServerError
           , Reader.MonadReader Environment
           , Functor
           , Applicative
           , Monad
           , IO.MonadIO
           )

appMToHandler :: Environment -> AppM a -> Handler a
appMToHandler environment r = do
  eitherErrorOrResult <- IO.liftIO $ flip Reader.runReaderT environment . Except.runExceptT . unAppM $ r
  case eitherErrorOrResult of
    Left err     -> throwError err
    Right result -> return result
