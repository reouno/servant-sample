{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API12 where

import           Servant

type API12For a i =
    Get '[JSON] [a] -- list 'a's
    :<|> ReqBody '[JSON] a :> Post '[JSON] i -- add an 'a'
    :<|> Capture "id" i :>
        ( Get '[JSON] a -- view an 'a' given its 'identifier' of type 'i'
        :<|> ReqBody '[JSON] a :> Put '[JSON] Bool -- update an 'a'
        :<|> Delete '[JSON] Bool -- delete an 'a'
        )

api12For :: Proxy (API12For a i)
api12For = Proxy
