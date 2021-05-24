{-# LANGUAGE DerivingStrategies #-}

module Utils
    ( BemClass (..)
    , bemClass
    ) where

import Miso.String (MisoString (), ms)


data BemClass
    = BemClass ParentName [BlockModifier] [ElementModifier]
    deriving stock (Eq, Show)

bemClass :: BlockName -> BemClass -> MisoString
bemClass blockName (BemClass parentName blockModifiers elemModifiers) = ms
    $ blockName ++ " " ++ parentName ++ "-" ++ blockName
        ++ bemModifiers blockName blockModifiers
        ++ bemModifiers (parentName ++ "-" ++ blockName) elemModifiers

bemModifiers :: Name -> [Modifier] -> String
bemModifiers _    []         = ""
bemModifiers name modifiers  =
    ' ' : (unwords . map ((name ++ "_") ++) . filter (not . null) $ modifiers)

type BlockModifier   = Modifier
type BlockName       = Name
type ElementModifier = Modifier
type Modifier        = String
type Name            = String
type ParentName      = Name
