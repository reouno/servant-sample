module Server where

import Data.Aeson
import Servant

import API1 (UserAPI1, userAPI1, users1)

server1 :: Server UserAPI1
server1 = return users1

app1 :: Application
app1 = serve userAPI1 server1
