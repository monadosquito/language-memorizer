{-# LANGUAGE DerivingStrategies #-}

module Model.Action
    ( Action (..)
    ) where

import Miso (URI ())

import Model.Model (Set (), Unit ())
import Utils (SetIx ())


data Action
    = AddSet
    | AddUnit
    | ChangeUri URI
    | DeleteSet SetIx
    | DoNothing
    | HandleUri URI
    | RefreshSet SetIx
    | SaveSet SetIx
    | SaveSets
    | UpdateSets (Maybe SetIx) Set
    | UpdateUnits SetIx Unit
    deriving stock (Eq, Show)
