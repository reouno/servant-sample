{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API1 where

import Data.Aeson
import Data.Time.Calendar
import Servant

import DataTypes (User(..))

type UserAPI1 = "users" :> Get '[JSON] [User]

users1 :: [User]
users1 =
    [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein"  136 "ae@mc2.org" (fromGregorian 1905 12 1)
    ]

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy
