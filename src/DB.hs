module DB (withDBConnection) where

import qualified Database.PostgreSQL.Simple as PG

withDBConnection :: (PG.Connection -> IO a) -> IO a
withDBConnection f = do
  connection <- getDBConnection
  res <- f connection
  closeDBConnection connection
  return res

getDBConnection :: IO PG.Connection
getDBConnection =
  PG.connect PG.defaultConnectInfo { PG.connectDatabase = "weather"
                                   , PG.connectUser = "dev"
                                   , PG.connectPort = 5432
                                   , PG.connectPassword = ""
                                   }

closeDBConnection :: PG.Connection -> IO ()
closeDBConnection connection =
  PG.close connection
