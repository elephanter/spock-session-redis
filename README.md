# [spock-session-redis][]

Store [Spock](https://github.com/agrafix/Spock) session in Redis storage using [Hedis](https://github.com/informatikr/hedis) package

Session objects are serialized by aeson package.
Empty objects not stored in Redis

## To do:
- Set ttl for redis keys
- ss_toList, ss_filterSessions, ss_mapSessions are not implemented yet

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
import Web.Spock.Session.Redis.Config (defaultSpockCfg)

data Sess = SessionUserId T.Text | EmptySession deriving (Generic)
instance ToJSON Sess where
    toJSON (SessionUserId t) = String t
    toJSON EmptySession = Null
instance FromJSON Sess

main :: IO ()
main = do
    redisConnectionPool <- R.connect R.defaultConnectInfo
    scfg <- defaultSpockCfg redisConnectionPool EmptySession () ()
    S.runSpock 8080 (S.spock scfg app)

def app = undefined
```

[spock-session-redis]: https://github.com/githubuser/spock-session-redis
