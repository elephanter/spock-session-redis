{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.Session.Redis.Config (
  getSpockCfg
)
where

import qualified Database.Redis as R
import Data.Aeson (FromJSON, ToJSON)
import Web.Spock.Config (SpockCfg(..), SessionCfg(..), PoolOrConn, defaultSpockCfg, defaultSessionCfg)
import Web.Spock.Session.Redis.Storage (newRedisSessionStore)

getSessionConfig :: (FromJSON a, ToJSON a) =>
                    R.Connection -> a -> IO (SessionCfg conn a st)
getSessionConfig conn emptySession= do
  defSessCfg <- defaultSessionCfg emptySession
  store <- newRedisSessionStore conn
  return defSessCfg { sc_emptySession = emptySession
                    , sc_store = store}

getSpockCfg :: (ToJSON sess,
                FromJSON sess) =>
              R.Connection
              -> sess
              -> PoolOrConn conn
              -> st
              -> IO (SpockCfg conn sess st)
getSpockCfg sessConn sess dbConn st = do
  cfg <- defaultSpockCfg () dbConn st
  sessCfg <- getSessionConfig sessConn sess
  return cfg { spc_sessionCfg = sessCfg }
