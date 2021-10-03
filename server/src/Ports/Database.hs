module Ports.Database
    ( DbConnection (..)
    ) where

import Data.Int (Int64 ())

import Common (LanguageMemorizer ())


class DbConnection d where
    addLanguageMemorizer          :: d -> LanguageMemorizer -> IO Int64
    addSet                        :: d -> SetName -> IO SetId
    addTranslate                  :: d -> UnitId -> Text -> IO ()
    addUnit                       :: d -> SetId -> Text -> IO UnitId
    connect                       :: IO d
    getLanguageMemorizerId        :: d -> LanguageMemorizer -> IO (Maybe LanguageMemorizerId)
    getLanguageMemorizerIdAndName :: d -> LanguageMemorizer -> IO [(Int, String)]
    withTransaction               :: d -> IO () -> IO ()
type LanguageMemorizerId = Int
type SetName             = String
type SetId               = Int
type Text                = String
type UnitId              = Int
