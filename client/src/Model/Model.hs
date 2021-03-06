{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Model
    ( BeingDownloadedSet    (..)
    , DownloadedSet         (..)
    , EditedSet             (..)
    , LiteLanguageMemorizer (..)
    , LiteSet               (..)
    , Memorizing            (..)
    , MemorizingMode        (..)
    , Model                 (..)
    , Pages                 (..)
    , Pagination            (..)
    , SetResultStep         (..)
    , SetResult             (..)
    , SetsPagination        (..)
    , SetsType              (..)
    , Settings              (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import GHC.Generics (Generic ())
import Miso (URI ())
import Miso.String (MisoString ())

import Common (LiteSharedSet (), Set (), SharedSet (), Unit ())


newtype BeingDownloadedSet = BeingDownloadedSet { _sharedSet :: SharedSet }
    deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

newtype DownloadedSet = DownloadedSet { _sharedSet :: SharedSet }
    deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data EditedSet = EditedSet
    { _name      :: MisoString
    , _ixedUnits :: [(UnitIx, Unit)]
    } deriving (Eq, Show)

data MemorizingMode
    = Text
    | Translates
    deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data LiteLanguageMemorizer = LiteLanguageMemorizer
    { _langMemorizerId   :: Int
    , _langMemorizerName :: MisoString
    } deriving anyclass (FromJSON) deriving stock (Eq, Generic, Show)

data LiteSet = LiteSet
    { _setIx   :: Int
    , _unitIxs :: [Int]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data Model = Model
    { _activeSetIx       :: SetIx
    , _activeSetsType    :: SetsType
    , _editedSet         :: EditedSet
    , _dislikedSetsIds   :: [SetId]
    , _langMemorizerName :: Maybe LanguageMemorizerName
    , _likedSetsIds      :: [SetId]
    , _liteSharedSets    :: [LiteSharedSet]
    , _memorizing        :: Memorizing
    , _menuIsVisible     :: Bool
    , _pagination        :: Pagination
    , _sets              :: Sets
    , _settings          :: Settings
    , _statistics        :: [[SetResult]]
    , _uri               :: URI
    } deriving stock (Eq, Show)
type LanguageMemorizerName = MisoString
type Sets                  =
    [Either Set (Either SharedSet (Either BeingDownloadedSet DownloadedSet))]
type SetId                 = Int
type SetIx                 = Int
type UnitIx                = Int

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
    { _sets       :: SetsPagination
    , _statistics :: Pages
    , _units      :: [Pages]
    } deriving stock (Eq, Show)

data SetResultStep = SetResultStep
    { _success :: Bool
    , _unitIx  :: Int
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data SetResult = SetResult
    { _setIx :: Int
    , _steps :: [SetResultStep]
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)

data SetsPagination = SetsPagination
    { _downloaded  :: Pages
    , _myLocal     :: Pages
    , _myShared    :: Pages
    , _theirShared :: Pages
    } deriving stock (Eq, Show)

data SetsType = Downloaded | Local | MyShared | Shared deriving stock (Eq, Show)

data Settings = Settings
    { _activeSetIxs        :: Maybe [String]
    , _darkMode            :: Maybe String
    , _memorizingMode      :: MemorizingMode
    , _setsPageCount
    , _statisticsPageCount
    , _unitsPageCount      :: String
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
