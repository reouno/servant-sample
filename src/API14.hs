{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API14 where

import           Control.Monad.Reader
import           Servant

readerToHandler :: Reader String a -> Handler a
readerToHandler r = return (runReader r "hi")

type ReaderAPI14 =
    "a" :> Get '[JSON] Int
    :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI14 :: Proxy ReaderAPI14
readerAPI14 = Proxy
