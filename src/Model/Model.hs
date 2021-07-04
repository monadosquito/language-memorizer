{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Model
    ( EditedSet  (..)
    , Model      (..)
    , Pages      (..)
    , Pagination (..)
    , Set        (..)
    , Settings   (..)
    , Unit       (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import GHC.Generics (Generic ())
import Miso (URI ())
import Miso.String (MisoString ())

import Utils (SetIx (), UnitIx ())


data EditedSet = EditedSet
    { _name      :: MisoString
    , _ixedUnits :: [(UnitIx, Unit)]
    } deriving (Eq, Show)

data Model = Model
    { _activeSetIx :: SetIx
    , _editedSet   :: EditedSet
    , _pagination  :: Pagination
    , _sets        :: [Set]
    , _settings    :: Settings
    , _uri         :: URI
    } deriving stock (Eq, Show)

data Pages = Pages
    { _current
    , _count   :: Int
    } deriving stock (Eq, Show)

data Pagination = Pagination
    { _sets  :: Pages
    , _units :: [Pages]
    } deriving stock (Eq, Show)

data Set = Set
    { _name  :: MisoString
    , _units :: Maybe [Unit]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Settings = Settings
    { _unitsPageCount
    , _setsPageCount  :: String
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Unit = Unit
    { _text       :: MisoString
    , _translates :: [MisoString]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
