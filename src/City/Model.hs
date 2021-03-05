{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module City.Model where

import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as PG
import           GHC.Generics

data City = City
    { cityId   :: UUID
    , cityName :: String
    }
    deriving (Eq, Show, Generic, PG.FromRow)
