module Ports.Database
    ( DbConnection (..)
    ) where

import Data.Int (Int64 ())

import Common (LanguageMemorizer ())


class DbConnection d where
    addLanguageMemorizer          :: d -> LanguageMemorizer -> IO Int64
    addSet                        :: d -> LanguageMemorizerId -> SetName -> IO SetId
    addTranslate                  :: d -> UnitId -> Text -> IO ()
    addUnit                       :: d -> SetId -> Text -> IO UnitId
    connect                       :: IO d
    deleteSet                     :: d -> SetId -> IO ()
    getLanguageMemorizerId        :: d -> LanguageMemorizer -> IO (Maybe LanguageMemorizerId)
    getLanguageMemorizerIdAndName :: d -> LanguageMemorizer -> IO [(Int, String)]
    getSetOwnerId                 :: d -> SetId -> IO SetOwnerId
    withTransaction               :: d -> IO a -> IO a
type LanguageMemorizerId = Int
type SetName             = String
type SetId               = Int
type SetOwnerId          = LanguageMemorizerId
type Text                = String
type UnitId              = Int
