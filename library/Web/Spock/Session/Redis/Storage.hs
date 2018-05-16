{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.Session.Redis.Storage where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Control.Monad.IO.Class
import Data.Time (getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Database.Redis as R
import qualified Data.Text.Encoding as T
import Data.Aeson
import Data.Aeson.Types (Value(Null))
import Data.ByteString.Lazy (toStrict)

loadSession ::(FromJSON a) => SessionId -> R.Connection -> IO (Maybe (Session conn a st))
loadSession key conn = R.runRedis conn $ do
    curtime <- liftIO getCurrentTime
    val <- R.get ekey
    case either (const Nothing) (decodeStrict' =<<) val of
      Just decoded -> do
        ttl <- R.ttl ekey
        let expAt = either (const curtime) ((`addUTCTime` curtime) . fromInteger) ttl
        return $ Just $ Session key "" expAt decoded
      _ -> return Nothing
  where
    ekey = T.encodeUtf8 key


deleteSession :: SessionId -> R.Connection -> IO ()
deleteSession key conn = R.runRedis conn $ do
  _ <- R.del [T.encodeUtf8 key]
  return ()

storeSession :: (ToJSON a) => Session conn a st -> R.Connection -> IO ()
storeSession s conn = R.runRedis conn $
  case toJSON (sess_data s) of
    Null -> return ()
    _    -> do
      _ <- R.mset [(key, s_val)]
      _ <- R.expireat key (round . utcTimeToPOSIXSeconds $ sess_validUntil s)
      return ()
  where
    key = T.encodeUtf8 $ sess_id s
    s_val = (toStrict . encode) $ sess_data s

toList :: R.Connection -> IO [Session conn a st]
toList _ = return []

newRedisSessionStore' ::(ToJSON a, FromJSON a) => R.Connection -> IO (SessionStore (Session conn a st) IO)
newRedisSessionStore' conn = return
  SessionStore
         { ss_runTx =  R.runRedis conn . liftIO
         , ss_loadSession = flip loadSession conn
         , ss_deleteSession = flip deleteSession conn
         , ss_storeSession = flip storeSession conn
         , ss_toList = toList conn
         , ss_filterSessions = undefined --flip filterSessions conn
         , ss_mapSessions = undefined --flip mapSessions conn
         }

newRedisSessionStore :: (ToJSON a, FromJSON a) => R.Connection -> IO (SessionStoreInstance (Session conn a st))
newRedisSessionStore rconn = SessionStoreInstance <$> newRedisSessionStore' rconn
