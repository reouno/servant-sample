module Server where

import           Data.Aeson
import           Servant

import           API1       (UserAPI1, userAPI1, users1)
import           API2       (UserAPI2, albert, isaac, userAPI2, users2)
import           API3       (VarAPI3, emailForClient, varAPI3)
import           DataTypes  (ClientInfo (..), Email (..), HelloMessage (..),
                             Position (..))

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

server3 :: Server VarAPI3
server3 = position
    :<|> hello
    :<|> marketing
    where
        position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
            Nothing   -> "Hello annonymous coward."
            Just name -> "Hello " ++ name ++ "."

        marketing :: ClientInfo -> Handler Email
        marketing clientInfo = return $ emailForClient clientInfo

app3 :: Application
app3 = serve varAPI3 server3
