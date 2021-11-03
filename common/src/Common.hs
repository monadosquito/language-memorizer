{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Common
    ( LanguageMemorizer (..)
    , LiteSharedSet     (..)
    , Set               (..)
    , SharedSet         (..)
    , Unit              (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import Database.PostgreSQL.Simple (FromRow ())
import GHC.Generics (Generic ())


data LanguageMemorizer = LanguageMemorizer
    { _email
    , _name
    , _password :: String
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data LiteSharedSet = LiteSharedSet
    { _sharedSetId   :: Int
    , _sharedSetName :: String
    } deriving anyclass (FromJSON, FromRow, ToJSON) deriving stock (Eq, Generic, Show)

data Set = Set
    { _name  :: String
    , _units :: Maybe [Unit]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data SharedSet = SharedSet
    { _id  :: Int
    , _set :: Set
    } deriving (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Unit = Unit
    { _text       :: String
    , _translates :: [String]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
