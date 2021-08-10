module Ports.Database
    ( DbConnection (..)
    ) where

import Data.Int (Int64 ())

import Core (LanguageMemorizer ())


class DbConnection d where
    addLanguageMemorizer          :: d -> LanguageMemorizer -> IO Int64
    connect                       :: IO d
    getLanguageMemorizerId        :: d -> LanguageMemorizer -> IO (Maybe LanguageMemorizerId)
    getLanguageMemorizerIdAndName :: d -> LanguageMemorizer -> IO [(Int, String)]
type LanguageMemorizerId    = Int
