{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API2 where

import           Data.Time.Calendar
import           Servant

import           DataTypes          (User (..))

type UserAPI2 =
    "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "isaac" :> Get '[JSON] User

albert :: User
albert = User "Albert Einstein"  136 "ae@mc2.org" (fromGregorian 1905 12 1)

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)

users2 :: [User]
users2 = [albert, isaac]

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy
