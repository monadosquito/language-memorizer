{-# LANGUAGE DerivingStrategies #-}

module Model.Action
    ( Action (..)
    ) where

data Action
    = DoNothing
    deriving stock (Eq, Show)
