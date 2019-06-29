{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import           Data.Aeson
import           Data.Time.Calendar
import           GHC.Generics

data User = User
    { name             :: String
    , age              :: Int
    , email            :: String
    , registrationDate :: Day
    } deriving (Eq, Generic, Show)

instance ToJSON User

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    } deriving (Eq, Generic, Show)

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
    deriving (Eq, Generic, Show)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
    { clientName         :: String
    , clientEmail        :: String
    , clientAge          :: Int
    , clientInterestedIn :: [String]
    } deriving (Eq, Generic, Show)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
    { from    :: String
    , to      :: String
    , subject :: String
    , body    :: String
    } deriving (Eq, Generic, Show)

instance ToJSON Email
