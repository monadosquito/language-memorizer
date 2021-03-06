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
    addDislike (PostgreSQLConn postgreSQLConn) (likedSetId, likerId) = DPS.execute
        postgreSQLConn
        "INSERT INTO \"dislike\" VALUES (?, ?) ON CONFLICT DO NOTHING"
        (likedSetId, likerId)

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

    addLike (PostgreSQLConn postgreSQLConn) (likedSetId, likerId) = DPS.execute
        postgreSQLConn
        "INSERT INTO \"like\" VALUES (?, ?) ON CONFLICT DO NOTHING"
        (likedSetId, likerId)

    addSet (PostgreSQLConn postgreSQLConn) langMemorizerId setName =
        (DPS.fromOnly . head) <$> (DPS.query
            postgreSQLConn
            "INSERT INTO set (owner_id, name) VALUES (?, ?) RETURNING id"
            (langMemorizerId, setName) :: IO [DPS.Only Int])

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

    deleteDislike (PostgreSQLConn postgreSQLConn) (likedSetId, likerId) = DPS.execute
        postgreSQLConn
        "DELETE FROM \"dislike\" WHERE disliked_set_id = ? AND disliker_id = ?"
        (likedSetId, likerId)

    deleteLike (PostgreSQLConn postgreSQLConn) (likedSetId, likerId) = DPS.execute
        postgreSQLConn
        "DELETE FROM \"like\" WHERE liked_set_id = ? AND liker_id = ?"
        (likedSetId, likerId)

    deleteSet (PostgreSQLConn postgreSQLConn) setId =
        void . DPS.execute postgreSQLConn "DELETE FROM set WHERE id = ?"
            $ DPS.Only setId

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

    getSetDislikes (PostgreSQLConn postgreSQLConn) setId = DPS.query
        postgreSQLConn
        "SELECT disliked_set_id, disliker_id FROM dislike WHERE disliked_set_id = ?"
        $ DPS.Only setId :: IO [(Int, Int)]

    getSetLikes (PostgreSQLConn postgreSQLConn) setId = DPS.query
        postgreSQLConn
        "SELECT liked_set_id, liker_id FROM \"like\" WHERE liked_set_id = ?"
        $ DPS.Only setId :: IO [(Int, Int)]

    getSetName (PostgreSQLConn postgreSQLConn) setId = (DPS.fromOnly . head)
        <$> (DPS.query
            postgreSQLConn
            "SELECT name FROM set WHERE id = ?"
            $ DPS.Only setId)

    getSetOwnerId (PostgreSQLConn postgreSQLConn) setId =
        (head . map DPS.fromOnly)
            <$> (DPS.query
                postgreSQLConn
                "SELECT owner_id FROM set WHERE id = ?"
                $ DPS.Only setId :: IO [DPS.Only Int])

    getSetUnits (PostgreSQLConn postgreSQLConn) setId = DPS.query
        postgreSQLConn "SELECT id, text FROM unit WHERE set_id = ?"
        $ DPS.Only setId

    getSetsIdsAndNames (PostgreSQLConn postgreSQLConn) = DPS.query_
        postgreSQLConn
        "SELECT id, name FROM set"

    getTranslatesTexts (PostgreSQLConn postgreSQLConn) unitId = (map DPS.fromOnly)
        <$> (DPS.query
            postgreSQLConn
            "SELECT text FROM translate WHERE unit_id = ?"
            $ DPS.Only unitId :: IO [DPS.Only String])

    withTransaction (PostgreSQLConn postgreSQLConn) postgreSQLAction =
        DPS.withTransaction postgreSQLConn postgreSQLAction

newtype PostgreSQLConn = PostgreSQLConn DPS.Connection
