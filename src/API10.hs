{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API10 where

import           Servant

-- API for values of type 'a'
type APIFor a =
    Get '[JSON] [a] -- list 'a's

apiFor :: Proxy (APIFor a)
apiFor = Proxy
