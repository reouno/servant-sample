{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module API5 where

import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Media   ((//), (/:))
import           Servant

data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
    mimeRender _ s = s

type IOAPI5 = "myfile.html" :> Get '[HTML] ByteString

ioAPI5 :: Proxy IOAPI5
ioAPI5 = Proxy
