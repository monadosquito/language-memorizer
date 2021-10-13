{-# LANGUAGE DerivingStrategies #-}

module Model.Action
    ( Action                (..)
    , EditedSetPart         (..)
    , MemorizingMode        (..)
    , Paginated             (..)
    , PaginationUpdatingWay (..)
    , PageSwitchWay         (..)
    ) where

import Miso (URI ())
import Miso.String (MisoString ())

import Common (Set (), SharedSet (), Unit ())
import Model.Model (Settings ())
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
    | DoNothing
    | EditSet EditedSetPart UnitIx EditedSetPartValue
    | FailMemorizingStep
    | HandleUri URI
    | RefreshSet SetIx
    | RepeatMemorizing
    | SaveSet
    | SaveSets
    | SaveSettings
    | SelectRandomMemorizingUnit
    | SetLanguageMemorizerName LanguageMemorizerName
    | SetNewSharedSetId SharedSetIx SharedSetId
    | ShareSet SetIx
    | ShowAnswer MemorizingMode
    | SignIn
    | SignOut
    | SignUp
    | SwitchPage PageSwitchWay Paginated
    | ToggleMenuVisibility
    | UpdatePagination PaginationUpdatingWay
    | UpdateSets (Maybe SetIx) (Either Set SharedSet)
    | UpdateSettings Settings
    | UpdateSharedSet SharedSetIx
    | UpdateMemorizing LiteSetIx TmpUnitIx TranslateIx
    | UpdateUnits SetIx Unit
    deriving stock (Eq, Show)
type Answer                = MisoString
type EditedSetPartValue    = MisoString
type LanguageMemorizerName = MisoString
type LiteSetIx             = Int
type SharedSetId           = Int
type SharedSetIx           = Int
type TmpUnitIx             = Int

data EditedSetPart = Name | UnitText | UnitTranslate TranslateIx deriving stock (Eq, Show)

type TranslateIx = Int

data MemorizingMode = Text' | Translates deriving stock (Eq, Show)

data PageSwitchWay = First | Jump Page | Last | Next | Previous deriving stock (Eq, Show)

data Paginated = Sets | Statistics | Units SetIx deriving stock (Eq, Show)

data PaginationUpdatingWay = Whole | Part Paginated deriving stock (Eq, Show)
