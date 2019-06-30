{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module API4 where

import           Lucid
import           Network.HTTP.Media ((//), (/:))
import           Servant

import           DataTypes          (Person (..))

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS

type PersonAPI4 = "persons" :> Get '[JSON, HTMLLucid] [Person]

people :: [Person]
people =
    [ Person "Isaac" "Newton"
    , Person "Albert" "Einsten"
    ]

personAPI4 :: Proxy PersonAPI4
personAPI4 = Proxy
