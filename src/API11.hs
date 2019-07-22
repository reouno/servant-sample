{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API11 where

import           Servant

type API11For a i =
    Get '[JSON] [a]
    :<|> ReqBody '[JSON] a :> Post '[JSON] i

api11For :: Proxy (API11For a i)
api11For = Proxy
