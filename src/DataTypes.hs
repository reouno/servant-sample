{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registrationDate :: Day
    } deriving (Eq, Generic, Show)

instance ToJSON User
