{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Servant
import           System.Directory               ( doesFileExist )

import           API1                           ( UserAPI1
                                                , userAPI1
                                                , users1
                                                )
import           API10                          ( APIFor
                                                , apiFor
                                                )
import           API11                          ( API11For
                                                , api11For
                                                )
import           API12                          ( API12For
                                                , api12For
                                                )
import           API13                          ( UsersProdsAPI13
                                                , usersProdsAPI13
                                                )
import           API14                          ( ReaderAPI14
                                                , readerAPI14
                                                , readerToHandler
                                                )
import           API2                           ( UserAPI2
                                                , albert
                                                , isaac
                                                , userAPI2
                                                , users2
                                                )
import           API3                           ( VarAPI3
                                                , emailForClient
                                                , varAPI3
                                                )
import           API4                           ( PersonAPI4
                                                , people
                                                , personAPI4
                                                )
import           API5                           ( IOAPI5
                                                , ioAPI5
                                                )
import           API6                           ( MayHeaderHandlerAPI6
                                                , albert6
                                                , mayHeaderHandlerAPI6
                                                )
import           API7                           ( StaticAPI7
                                                , staticAPI7
                                                )
import           API8                           ( User2API8
                                                , UserAPI8
                                                , user2API8
                                                , userAPI8
                                                )
import           API9                           ( UsersAPI9
                                                , matrixUsers
                                                , usersAPI9
                                                )
import           DataTypes                      ( ClientInfo(..)
                                                , Email(..)
                                                , HelloMessage(..)
                                                , Position(..)
                                                , Product(..)
                                                , User
                                                )

server1 :: Server UserAPI1
server1 = return users1

app1 :: Application
app1 = serve userAPI1 server1

server2 :: Server UserAPI2
server2 = return users2 :<|> return albert :<|> return isaac

app2 :: Application
app2 = serve userAPI2 server2

server3 :: Server VarAPI3
server3 = position :<|> hello :<|> marketing
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

server4 :: Server PersonAPI4
server4 = return people

app4 :: Application
app4 = serve personAPI4 server4

server5 :: Server IOAPI5
server5 = do
  exists <- liftIO (doesFileExist "myfile.html")
  if exists then liftIO (B.readFile "myfile.html") else throwError custom404Err
  where custom404Err = err404 { errBody = "myfile.html does not exist." }

app5 :: Application
app5 = serve ioAPI5 server5

-- `b` is passed from request capturing "withHeader"
server6 :: Server MayHeaderHandlerAPI6
server6 b = return $ if b then addHeader 1797 albert6 else noHeader albert6

app6 :: Application
app6 = serve mayHeaderHandlerAPI6 server6

server7 :: Server StaticAPI7
server7 = serveDirectoryWebApp "static-files"

app7 :: Application
app7 = serve staticAPI7 server7

server8 :: Server UserAPI8
server8 = getUser :<|> deleteUser
 where
  getUser :: Int -> Handler User
  getUser _userid = error "getUesr..."

  deleteUser :: Int -> Handler NoContent
  deleteUser _userid = error "deleteUser..."

app8 :: Application
app8 = serve userAPI8 server8

-- notice how getUser and deleteUser
-- have a different type! no argument anymore,
-- the argument directly goes to the whole Server
server8' :: Server User2API8
server8' userid = getUser userid :<|> deleteUser userid
 where
  getUser :: Int -> Handler User
  getUser = error "getUser..."

  deleteUser :: Int -> Handler NoContent
  deleteUser = error "deleteUser..."

app8' :: Application
app8' = serve user2API8 server8'

server9 :: Server UsersAPI9
server9 = getUsers :<|> newUser :<|> userOperations
 where
  getUsers :: Handler [User]
  getUsers = return matrixUsers

  newUser :: User -> Handler String
  newUser user = return "receive post request to add new user"

  userOperations userid =
    viewUser userid :<|> updateUser userid :<|> deleteUser userid
   where
    viewUser :: Int -> Handler User
    viewUser userid = return $ matrixUsers !! userid

    updateUser :: Int -> User -> Handler String
    updateUser userid user =
      return
        $  "received PUT request to update the user with id = "
        ++ show userid

    deleteUser :: Int -> Handler String
    deleteUser userid =
      return $ "received DELETE request with userid = " ++ show userid

app9 :: Application
app9 = serve usersAPI9 server9

server10For :: Handler [a] -> Server (APIFor a)
server10For viewData = viewData

server10MatrixUsers :: Server (APIFor User)
server10MatrixUsers = server10For viewUsers
 where
  viewUsers :: Handler [User]
  viewUsers = return matrixUsers

app10MatrixUsers :: Application
app10MatrixUsers = serve apiFor server10MatrixUsers

server11For :: Handler [a] -> (a -> Handler i) -> Server (API11For a i)
server11For viewData addDatum = viewData :<|> addDatum

server11MatrixUsers :: Server (API11For User Int)
server11MatrixUsers = server11For viewUsers addUser
 where
  viewUsers :: Handler [User]
  viewUsers = return matrixUsers

  addUser :: User -> Handler Int
  addUser user = return 100

app11MatrixUsers :: Application
app11MatrixUsers = serve api11For server11MatrixUsers

server12For
  :: Handler [a]
  -> (a -> Handler i)
  -> (i -> Handler a)
  -> (i -> a -> Handler Bool)
  -> (i -> Handler Bool)
  -> Server (API12For a i)
server12For viewAll add view update delete = viewAll :<|> add :<|> operations
  where operations id' = view id' :<|> update id' :<|> delete id'

products :: [Product]
products =
  [ Product "MacBook Air" 1   "PC"
  , Product "MacBook Pro" 100 "PC"
  , Product "Chair"       3   "Furniture"
  ]

server12Products :: Server (API12For Product Int)
server12Products = server12For viewProds add view update delete
 where
  viewProds :: Handler [Product]
  viewProds = return products

  add :: Product -> Handler Int
  add prod = return 3

  view :: Int -> Handler Product
  view id' = return $ products !! id'

  update :: Int -> Product -> Handler Bool
  update id' prod = return True

  delete :: i -> Handler Bool
  delete id' = return True

app12Products :: Application
app12Products = serve api12For server12Products

serverUsersProdsAPI13 :: Server UsersProdsAPI13
serverUsersProdsAPI13 = usersOps :<|> productsOps :<|> emptyServer
 where
  usersOps    = viewUsers :<|> addUser :<|> userOps
  productsOps = viewProducts :<|> addProduct :<|> productOps

  viewUsers :: Handler [User]
  viewUsers = return matrixUsers

  addUser :: User -> Handler Int
  addUser _ = return 123

  userOps id' = viewUser id' :<|> updateUser id' :<|> deleteUser id'

  viewUser :: Int -> Handler User
  viewUser id' = return $ matrixUsers !! id'

  updateUser :: Int -> User -> Handler Bool
  updateUser _ _ = return True

  deleteUser :: Int -> Handler Bool
  deleteUser _ = return True

  viewProducts :: Handler [Product]
  viewProducts = return products

  addProduct :: Product -> Handler Int
  addProduct prod = return 999

  productOps id' =
    viewProduct id' :<|> updateProduct id' :<|> deleteProduct id'

  viewProduct :: Int -> Handler Product
  viewProduct id' = return $ products !! id'

  updateProduct :: Int -> Product -> Handler Bool
  updateProduct _ _ = return True

  deleteProduct :: Int -> Handler Bool
  deleteProduct _ = return True

app13UsersProds :: Application
app13UsersProds = serve usersProdsAPI13 serverUsersProdsAPI13

serverTReaderAPI14 :: ServerT ReaderAPI14 (Reader String)
serverTReaderAPI14 = a :<|> b
 where
  a :: Reader String Int
  a = return 1979

  b :: Double -> Reader String Bool
  b _ = asks (== "hi")

serverReaderAPI14 :: Server ReaderAPI14
serverReaderAPI14 = hoistServer readerAPI14 readerToHandler serverTReaderAPI14

app14Reader :: Application
app14Reader = serve readerAPI14 serverReaderAPI14
