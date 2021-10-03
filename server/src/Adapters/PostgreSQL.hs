{-# LANGUAGE OverloadedStrings #-}

module Adapters.PostgreSQL
    ( PostgreSQLConn (..)
    ) where

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import System.Environment (getEnv)

import Common (LanguageMemorizer (LanguageMemorizer))
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

    addSet (PostgreSQLConn postgreSQLConn) setName =
        (DPS.fromOnly . head) <$> (DPS.query
            postgreSQLConn
            "INSERT INTO set (name) VALUES (?) RETURNING id"
            $ DPS.Only setName :: IO [DPS.Only Int])

    addTranslate (PostgreSQLConn postgreSQLConn) unitId text = void $ DPS.execute
        postgreSQLConn
        "INSERT INTO translate (unit_id, text) VALUES (?, ?)"
        (unitId, text)

    addUnit (PostgreSQLConn postgreSQLConn) setId text =
        (DPS.fromOnly . head) <$> (DPS.query
            postgreSQLConn
            "INSERT INTO unit (set_id, text) VALUES (?, ?) RETURNING id"
            (setId, text) :: IO [DPS.Only Int])

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

    withTransaction (PostgreSQLConn postgreSQLConn) postgreSQLAction =
        DPS.withTransaction postgreSQLConn postgreSQLAction

newtype PostgreSQLConn = PostgreSQLConn DPS.Connection
