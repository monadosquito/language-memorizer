{-# LANGUAGE DerivingStrategies #-}

module Model.Action
    ( Action                (..)
    , EditedSetPart         (..)
    , Paginated             (..)
    , PaginationUpdatingWay (..)
    , PageSwitchWay         (..)
    ) where

import Miso (URI ())
import Miso.String (MisoString ())

import Model.Model (Set (), Settings (), Unit ())
import Utils (Page (), SetIx (), UnitIx ())


data Action
    = AddSet
    | AddUnit
    | AddTranslate UnitIx
    | ChangeUri URI
    | DeleteSet SetIx
    | DeleteTranslate UnitIx TranslateIx
    | DeleteUnit UnitIx
    | DoNothing
    | EditSet EditedSetPart UnitIx EditedSetPartValue
    | HandleUri URI
    | RefreshSet SetIx
    | SaveSet
    | SaveSets
    | SaveSettings
    | SwitchPage PageSwitchWay Paginated
    | UpdatePagination PaginationUpdatingWay
    | UpdateSets (Maybe SetIx) Set
    | UpdateSettings Settings
    | UpdateUnits SetIx Unit
    deriving stock (Eq, Show)
type EditedSetPartValue = MisoString

data EditedSetPart = Name | UnitText | UnitTranslate TranslateIx deriving stock (Eq, Show)
type TranslateIx = Int

data PageSwitchWay = First | Jump Page | Last | Next | Previous deriving stock (Eq, Show)

data Paginated = Sets | Units SetIx deriving stock (Eq, Show)

data PaginationUpdatingWay = Whole | Part Paginated deriving stock (Eq, Show)
