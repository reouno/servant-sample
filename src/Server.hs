module Server where

import           Data.Aeson
import           Servant

import           API1       (UserAPI1, userAPI1, users1)
import           API2       (UserAPI2, albert, isaac, userAPI2, users2)

server1 :: Server UserAPI1
server1 = return users1

app1 :: Application
app1 = serve userAPI1 server1

server2 :: Server UserAPI2
server2 = return users2
    :<|> return albert
    :<|> return isaac

app2 :: Application
app2 = serve userAPI2 server2
