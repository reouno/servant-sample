{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API8 where

import           Servant

import           DataTypes (User)

type UserAPI8 = Capture "userid" Int :> Get '[JSON] User
    :<|> Capture "userid" Int :> DeleteNoContent '[JSON] NoContent

userAPI8 :: Proxy UserAPI8
userAPI8 = Proxy

type User2API8 = Capture "userid" Int :>
    (
        Get '[JSON] User
        :<|> DeleteNoContent '[JSON] NoContent
    )

user2API8 :: Proxy User2API8
user2API8 = Proxy
