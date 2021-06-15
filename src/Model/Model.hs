{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}

module Model.Model
    ( Model (..)
    , Set   (..)
    , Unit  (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import GHC.Generics (Generic ())
import Miso (URI ())
import Miso.String (MisoString ())


data Model = Model
    { _sets :: [Set]
    , _uri  :: URI
    } deriving stock (Eq, Show)

data Set = Set
    { _name  :: MisoString
    , _units :: Maybe [Unit]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Unit = Unit
    { _text       :: MisoString
    , _translates :: [MisoString]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
