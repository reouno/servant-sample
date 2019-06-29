{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API3 where

import           Data.List (intercalate)
import           Servant

import           DataTypes (ClientInfo (..), Email (..), HelloMessage (..),
                            Position (..))

type VarAPI3 =
    "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
    where
        from' = "great@company.com"
        to' = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body' = "Hi " ++ clientName c ++ ",\n\n"
            ++ "Since you've recently turned " ++ show (clientAge c)
            ++ ", have you checked out our latest "
            ++ intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"

varAPI3 :: Proxy VarAPI3
varAPI3 = Proxy
