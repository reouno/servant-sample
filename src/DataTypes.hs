{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import           Data.Aeson
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid

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

data Person = Person
    { firstName :: String
    , lastName  :: String
    } deriving (Eq, Generic, Show)

instance ToJSON Person

-- HTML serialization of single person
instance ToHtml Person where
    toHtml person =
        tr_ $ do
            td_ (toHtml $ firstName person)
            td_ (toHtml $ lastName person)

    -- do not worry about this
    toHtmlRaw = toHtml

instance ToHtml [Person] where
    toHtml persons = table_ $ do
        tr_ $ do
            th_ "first name"
            th_ "last name"

        foldMap toHtml persons

    toHtmlRaw = toHtml
