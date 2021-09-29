{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Common
    ( LanguageMemorizer (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import GHC.Generics (Generic ())


data LanguageMemorizer = LanguageMemorizer
    { _email
    , _name
    , _password :: String
    } deriving anyclass (FromJSON, ToJSON) deriving stock (Eq, Generic, Show)
