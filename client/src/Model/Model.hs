{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Model
    ( EditedSet         (..)
    , LanguageMemorizer (..)
    , LiteSet           (..)
    , Memorizing        (..)
    , MemorizingMode    (..)
    , Model             (..)
    , Pages             (..)
    , Pagination        (..)
    , SetResultStep     (..)
    , Set               (..)
    , SetResult         (..)
    , Settings          (..)
    , Unit              (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import GHC.Generics (Generic ())
import Miso (URI ())
import Miso.String (MisoString ())


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
    , _langMemorizer :: Maybe LanguageMemorizer
    , _memorizing    :: Memorizing
    , _menuIsVisible :: Bool
    , _pagination    :: Pagination
    , _sets          :: [Set]
    , _settings      :: Settings
    , _statistics    :: [[SetResult]]
    , _uri           :: URI
    } deriving stock (Eq, Show)
type SetIx  = Int
type UnitIx = Int

data LanguageMemorizer = LanguageMemorizer
    { _email
    , _name
    , _password :: String
    } deriving stock (Eq, Generic, Show) deriving anyclass (FromJSON, ToJSON)

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
    { _sets       :: Pages
    , _statistics :: Pages
    , _units      :: [Pages]
    } deriving stock (Eq, Show)

data SetResultStep = SetResultStep
    { _success :: Bool
    , _unitIx  :: Int
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Set = Set
    { _name  :: MisoString
    , _units :: Maybe [Unit]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data SetResult = SetResult
    { _setIx :: Int
    , _steps :: [SetResultStep]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Settings = Settings
    { _activeSetIxs        :: Maybe [String]
    , _darkMode            :: Maybe String
    , _memorizingMode      :: MemorizingMode
    , _setsPageCount
    , _statisticsPageCount
    , _unitsPageCount      :: String
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Unit = Unit
    { _text       :: MisoString
    , _translates :: [MisoString]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
