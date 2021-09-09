{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Model
    ( EditedSet      (..)
    , LiteSet        (..)
    , Memorizing     (..)
    , MemorizingMode (..)
    , Model          (..)
    , Pages          (..)
    , Pagination     (..)
    , Set            (..)
    , Settings       (..)
    , Unit           (..)
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

data MemorizingMode
    = Text
    | Translates
    deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data LiteSet = LiteSet
    { _setIx   :: Int
    , _unitIxs :: [Int]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Model = Model
    { _activeSetIx   :: SetIx
    , _editedSet     :: EditedSet
    , _memorizing    :: Memorizing
    , _menuIsVisible :: Bool
    , _pagination    :: Pagination
    , _sets          :: [Set]
    , _settings      :: Settings
    , _uri           :: URI
    } deriving stock (Eq, Show)

data Memorizing = Memorizing
    { _answer          :: MisoString
    , _liteSets        :: [LiteSet]
    , _pause           :: Bool
    , _progress        :: [Bool]
    , _initLiteSetsLen
    , _setIx
    , _translateIx
    , _unitIx          :: Int
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

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
    { _activeSetIxs   :: Maybe [String]
    , _memorizingMode :: MemorizingMode
    , _setsPageCount
    , _unitsPageCount :: String
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Unit = Unit
    { _text       :: MisoString
    , _translates :: [MisoString]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
