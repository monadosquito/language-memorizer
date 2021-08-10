{-# LANGUAGE OverloadedStrings #-}

module Adapters.PostgreSQL
    ( PostgreSQLConn (..)
    ) where

import Data.Maybe (listToMaybe)
import System.Environment (getEnv)

import Core (LanguageMemorizer (LanguageMemorizer))
import Ports.Database (DbConnection (..))

import qualified Database.PostgreSQL.Simple as DPS


instance DbConnection PostgreSQLConn where
    addLanguageMemorizer
        (PostgreSQLConn postgreSQLConn)
        (LanguageMemorizer email name password)
        =
        DPS.execute
            postgreSQLConn
            "INSERT INTO lang_memorizer (email, name, password)\
            \VALUES (?, ?, ?)\
            \ON CONFLICT DO NOTHING"
            (email, name, password)

    connect = do
        databaseHost <- getEnv "database_host"
        databaseName <- getEnv "database_name"
        databasePort <- getEnv "database_port"
        databaseUserName <- getEnv "database_user_name"
        databaseUserPassword <- getEnv "database_user_password"
        PostgreSQLConn <$> (DPS.connect $ DPS.defaultConnectInfo
            { DPS.connectDatabase = databaseName
            , DPS.connectHost     = databaseHost
            , DPS.connectPassword = databaseUserPassword
            , DPS.connectPort     = read databasePort
            , DPS.connectUser     = databaseUserName
            })

    getLanguageMemorizerId
        (PostgreSQLConn postgreSQLConn)
        (LanguageMemorizer email _ password)
        =
        (listToMaybe . map DPS.fromOnly) <$> (DPS.query
            postgreSQLConn
            "SELECT id FROM lang_memorizer WHERE email = ? AND password = ?"
            (email, password) :: IO [DPS.Only Int])

    getLanguageMemorizerIdAndName
        (PostgreSQLConn postgreSQLConn)
        (LanguageMemorizer email _ password)
        = DPS.query
            postgreSQLConn
            "SELECT id, name FROM lang_memorizer WHERE email = ? AND password = ?"
            (email, password)

newtype PostgreSQLConn = PostgreSQLConn DPS.Connection
