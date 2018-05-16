# [spock-session-redis][]

Store [Spock](https://github.com/agrafix/Spock) session in Redis storage using [Hedis](https://github.com/informatikr/hedis) package

Session objects are serialized by aeson package.
Empty objects not stored in Redis

example usage:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.Types
import qualified Web.Spock as S
import Web.Spock.Config hiding (defaultSpockCfg)
import qualified Data.Text as T
import qualified Database.Redis as R
import Web.Spock.Session.Redis.Config (getSpockCfg)

data Sess = SessionUserId T.Text | EmptySession deriving (Generic)
instance ToJSON Sess where
    toJSON (SessionUserId t) = String t
    toJSON EmptySession = Null
instance FromJSON Sess

main :: IO ()
main = do
    redisConnectionPool <- R.connect R.defaultConnectInfo
    scfg <- getSpockCfg redisConnectionPool EmptySession () ()
    S.runSpock 8080 (S.spock scfg app)

def app = do
    mgr <- S.getSessMgr
    liftIO $ SM.sm_closeSessionManager mgr
    ...
```

sm_closeSessionManager is called because we need shut down Spock housekeepThread. Because now we have redis watching session ttl for us.

[spock-session-redis]: https://github.com/githubuser/spock-session-redis
