{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API9 where

import           Data.Time.Calendar (fromGregorian)
import           Servant

import           DataTypes          (User (..))

neo :: User
neo = User "Neo" 57 "neo@matrix.world" (fromGregorian 1962 3 11)

trinity :: User
trinity = User "Trinity" 57 "trinity@matrix.com" (fromGregorian 1962 9 1)

matrixUsers :: [User]
matrixUsers = [neo, trinity]

type UsersAPI9 =
    "users" :> Get '[JSON] [User] -- list users
    :<|> ReqBody '[JSON] User :> Post '[JSON] String -- add a user
    :<|> Capture "userid" Int :>
        (
            Get '[JSON] User -- view a user
            :<|> ReqBody '[JSON] User :> Put '[JSON] String -- update a user
            :<|> Delete '[JSON] String -- delete a user
        )

usersAPI9 :: Proxy UsersAPI9
usersAPI9 = Proxy
