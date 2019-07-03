{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API7 where

import           Servant

type StaticAPI7 = "static" :> Raw

staticAPI7 :: Proxy StaticAPI7
staticAPI7 = Proxy
