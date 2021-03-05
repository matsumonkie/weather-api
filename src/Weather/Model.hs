{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module Weather.Model where

import qualified Data.Aeson.Types                   as JSON
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as PG
import           GHC.Generics

data Weather = Weather
    { weatherId   :: UUID
    , weatherName :: String
    }
    deriving (Eq, Show, Generic, PG.FromRow, JSON.FromJSON,
          JSON.ToJSON)
