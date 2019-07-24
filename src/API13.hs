{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API13 where

import           Servant

import           API12     (API12For)
import           DataTypes (Product (..), User (..))

type UsersProdsAPI13 =
    "users" :> (API12For User Int)
    :<|> "produts" :> (API12For Product Int)

usersProdsAPI13 :: Proxy UsersProdsAPI13
usersProdsAPI13 = Proxy
