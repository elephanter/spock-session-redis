{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.Session.Redis.Config where

import Data.Monoid
import qualified Database.Redis as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import Web.Spock
import Web.Spock.Config (SessionCfg(..), defaultSessionHooks, SpockCfg(..), PoolOrConn)

import Web.Spock.Session.Redis.Storage (newRedisSessionStore)

getSessionConfig :: (FromJSON a, ToJSON a) =>
                    R.Connection -> a -> IO (SessionCfg conn a st)
getSessionConfig conn emptySession= do
  store <- newRedisSessionStore conn
  return
    SessionCfg
    { sc_cookieName = "spockcookie"
    , sc_sessionTTL = 3600
    , sc_sessionIdEntropy = 64
    , sc_sessionExpandTTL = True
    , sc_emptySession = emptySession
    , sc_store = store
    , sc_housekeepingInterval = 60 * 10
    , sc_hooks = defaultSessionHooks
    }

defaultSpockCfg :: (FromJSON sess, ToJSON sess) =>
                   R.Connection
                   -> sess -> PoolOrConn conn -> st -> IO (SpockCfg conn sess st)
defaultSpockCfg sessConn sess dbConn st = do
  sessCfg <- getSessionConfig sessConn sess
  return
    SpockCfg
    { spc_initialState = st
    , spc_database = dbConn
    , spc_sessionCfg = sessCfg
    , spc_maxRequestSize = Just (5 * 1024 * 1024)
    , spc_errorHandler = defaultErrorHandler
    , spc_csrfProtection = False
    , spc_csrfHeaderName = "X-Csrf-Token"
    , spc_csrfPostName = "__csrf_token"
    }

defaultErrorHandler :: Status -> ActionCtxT () IO ()
defaultErrorHandler status = html $ defaultErrorTemplate status

defaultErrorTemplate :: Status -> T.Text
defaultErrorTemplate s =
  "<html><head>" <> "<title>" <> message <> "</title>" <> "</head>" <>
  "<body>" <>
  "<h1>" <> message <> "</h1>" <>
  "<a href='https://www.spock.li'>powered by Spock</a>" <>
  "</body>"
  where
    message = showT (statusCode s) <> " - " <> TE.decodeUtf8 (statusMessage s)
    showT = T.pack . show
