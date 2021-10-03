{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Core
    ( Set  (..)
    , Unit (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import GHC.Generics (Generic ())


data Set = Set
    { _name  :: String
    , _units :: Maybe [Unit]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Unit = Unit
    { _text       :: String
    , _translates :: [String]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
