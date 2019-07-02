{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API6 where

import           Data.Time.Calendar
import           Servant

import           DataTypes          (User (..))

albert6 :: User
albert6 = User "Albert Einstein"  136 "ae@mc2.org" (fromGregorian 1905 12 1)

type MayHeaderHandlerAPI6 = "user"
    :> Capture "withHeader" Bool
    :> Get '[JSON] (Headers '[Header "X-A-Int" Int] User)

mayHeaderHandlerAPI6 :: Proxy MayHeaderHandlerAPI6
mayHeaderHandlerAPI6 = Proxy
