{-# LANGUAGE DerivingStrategies #-}

module Model.Action
    ( Action                (..)
    , EditedSetPart         (..)
    , MemorizingMode        (..)
    , Paginated             (..)
    , PaginationUpdatingWay (..)
    , PageSwitchWay         (..)
    , SetsType              (..)
    ) where

import Miso (URI ())
import Miso.String (MisoString ())

import Common (LiteSharedSet (), Set (), SharedSet (), Unit ())
import Model.Model (BeingDownloadedSet (), DownloadedSet (), Settings ())
import Utils (Page (), SetIx (), UnitIx ())


data Action
    = AddSet
    | AddUnit
    | AddTranslate UnitIx
    | ChangeUri URI
    | CheckAnswer MemorizingMode Answer
    | DeleteSet SetIx
    | DeleteTranslate UnitIx TranslateIx
    | DeleteUnit UnitIx
    | DislikeSharedSet SharedSetIx
    | DoNothing
    | DownloadSharedSet SetIx
    | EditSet EditedSetPart UnitIx EditedSetPartValue
    | FailMemorizingStep
    | HandleUri URI
    | LikeSharedSet SharedSetIx
    | RefreshSet SetIx
    | RepeatMemorizing
    | SaveSet
    | SaveSets
    | SaveSettings
    | SelectRandomMemorizingUnit
    | SetDownloadedSet
    | SetLanguageMemorizerName LanguageMemorizerName
    | SetLiteSharedSets [LiteSharedSet]
    | SetNewSharedSetId SharedSetIx SharedSetId
    | ShareSet SetIx
    | ShowAnswer MemorizingMode
    | SignIn
    | SignOut
    | SignUp
    | SwitchActiveSetsType SetsType
    | SwitchPage PageSwitchWay Paginated
    | ToggleMenuVisibility
    | UnshareSet SharedSetIx
    | UpdatePagination PaginationUpdatingWay
    | UpdateSets GoingSet (Maybe SetsType) (Maybe SetIx) Set'
    | UpdateSettings Settings
    | UpdateSharedSet SharedSetIx
    | UpdateMemorizing LiteSetIx TmpUnitIx TranslateIx
    | UpdateUnits SetIx Unit
    deriving stock (Eq, Show)
type Answer                = MisoString
type EditedSetPartValue    = MisoString
type GoingSet              = Bool
type LanguageMemorizerName = MisoString
type LiteSetIx             = Int
type Set'                  =
    Either Set (Either SharedSet (Either BeingDownloadedSet DownloadedSet))
type SharedSetId           = Int
type SharedSetIx           = Int
type TmpUnitIx             = Int

data EditedSetPart = Name | UnitText | UnitTranslate TranslateIx deriving stock (Eq, Show)

type TranslateIx = Int

data MemorizingMode = Text' | Translates deriving stock (Eq, Show)

data PageSwitchWay = First | Jump Page | Last | Next | Previous deriving stock (Eq, Show)

data Paginated = Sets SetsType | Statistics | Units SetIx deriving stock (Eq, Show)

data PaginationUpdatingWay = Whole | Part Paginated deriving stock (Eq, Show)

data SetsType = Downloaded | MyLocal | MyShared | TheirShared deriving stock (Eq, Show)
