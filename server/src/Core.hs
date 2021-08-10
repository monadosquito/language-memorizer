{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Core
    ( LanguageMemorizer (..)
    ) where

import Data.Aeson (FromJSON (), ToJSON ())
import Database.PostgreSQL.Simple (FromRow ())
import GHC.Generics (Generic ())


data LanguageMemorizer = LanguageMemorizer
    { _email
    , _name
    , _password :: String
    } deriving anyclass (FromJSON, FromRow, ToJSON) deriving stock (Eq, Generic, Show)
