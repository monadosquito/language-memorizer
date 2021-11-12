{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TupleSections      #-}

module Adapters.Servant
    ( Servant (..)
    , run
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (), ToJSON ())
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.UTF8 (fromString)
import Data.CaseInsensitive (mk)
import GHC.Generics (Generic ())
import System.Environment (getEnv)

import Ports.Database (DbConnection (..))
import Ports.WwwServer (WwwServer (..))

import qualified Common as C
import qualified Data.ByteString.Char8 as DBC
import qualified Network.Wai.Handler.Warp as NWHW
import qualified Network.Wai.Middleware.Cors as NWMC
import qualified Servant as S
import qualified Servant.Auth.Server as SAS


type Api
    =      "get-shared-set" S.:> S.Capture "shared-set-id" Int S.:>
        S.Get '[S.JSON] C.SharedSet
    S.:<|> "get-shared-sets-ids-and-names" S.:> S.Get '[S.JSON] [C.LiteSharedSet]
    S.:<|> "sign-in" S.:> S.ReqBody '[S.JSON] C.LanguageMemorizer
        S.:> S.Post '[S.JSON] (String, String)
    S.:<|> "sign-up" S.:> S.ReqBody '[S.JSON] C.LanguageMemorizer
        S.:> S.PostNoContent '[S.PlainText] S.NoContent
    S.:<|> SAS.Auth '[SAS.JWT] AuthTokenPayload
        S.:> "share-set" S.:> S.ReqBody '[S.JSON] C.Set
            S.:> S.Post '[S.PlainText] String
    S.:<|> SAS.Auth '[SAS.JWT] AuthTokenPayload
        S.:> "unshare-set" S.:> S.Capture "shared-set-id" Int
            S.:> S.DeleteNoContent '[S.PlainText] S.NoContent
    S.:<|> SAS.Auth '[SAS.JWT] AuthTokenPayload
        S.:> "update-shared-set" S.:> S.ReqBody '[S.JSON] C.SharedSet
            S.:> S.Post '[S.PlainText] String
    S.:<|> S.Verb 'S.OPTIONS 200 '[S.PlainText] S.NoContent

data AuthTokenPayload = AuthTokenPayload
    { _id :: Int
    } deriving anyclass (FromJSON, SAS.FromJWT, SAS.ToJWT, ToJSON)
      deriving stock (Eq, Generic, Show)

data Servant = Servant deriving (Eq, Show)

instance WwwServer Servant where
    run dbCon = do
        allowedReqHeaders <- getEnv "allowed_request_headers"
        allowedReqMethods <- getEnv "allowed_request_methods"
        allowedReqOrigs <- getEnv "allowed_request_origins"
        jwtKey <- SAS.generateKey
        let
            corsPolicy = NWMC.simpleCorsResourcePolicy
                { NWMC.corsMethods        = DBC.words $ fromString allowedReqMethods
                , NWMC.corsOrigins        =
                    if allowedReqOrigs == "*"
                    then Nothing
                    else Just (DBC.words $ fromString allowedReqOrigs, False)
                , NWMC.corsRequestHeaders =
                    map mk . DBC.words $ fromString allowedReqHeaders
                }
            ctx         = SAS.defaultCookieSettings S.:. jwtSettings S.:. S.EmptyContext
            jwtSettings = SAS.defaultJWTSettings jwtKey
        port <- getEnv "api_server_port"
        NWHW.run (read port)
            . NWMC.cors (const $ Just corsPolicy)
            . S.serveWithContext (S.Proxy :: S.Proxy Api) ctx
            $ server dbCon jwtSettings
        pure Servant
      where

server :: DbConnection d => d -> SAS.JWTSettings -> S.Server Api
server dbConn jwtSettings
    =      getSharedSet
    S.:<|> getSharedSetsIdsAndNames
    S.:<|> signIn
    S.:<|> signUp
    S.:<|> shareSet
    S.:<|> unshareSet
    S.:<|> updateSharedSet
    S.:<|> corsOptions
  where
    corsOptions = pure S.NoContent
    getSharedSet sharedSetId = do
        unitsIdsAndTexts <- liftIO $ getSetUnits dbConn sharedSetId
        units <- liftIO $ mapM
            (\(unitId, unitText) ->
                pure . C.Unit unitText =<< getTranslatesTexts dbConn unitId)
            unitsIdsAndTexts
        sharedSetName <- liftIO $ getSetName dbConn sharedSetId
        pure . C.SharedSet sharedSetId . C.Set sharedSetName $ Just units
    getSharedSetsIdsAndNames = liftIO $ getSetsIdsAndNames dbConn
    shareSet
        (SAS.Authenticated (AuthTokenPayload langMemorizerId))
        (C.Set name mdUnits)
        = do
        liftIO . withTransaction dbConn $ do
            sharedSetId <- addSet dbConn langMemorizerId name
            addUnitsWithTrans sharedSetId mdUnits
            pure $ show sharedSetId
    shareSet _ _ = S.throwError S.err401
    signIn langMemorizer = do
        langMemorizersIdAndNames <- liftIO
            $ getLanguageMemorizerIdAndName dbConn langMemorizer
        if null langMemorizersIdAndNames
            then S.throwError S.err404
            else
                let
                    ((landMemorizerId, name):_) = langMemorizersIdAndNames
                in
                    liftIO $ ((name, ) . unpack . either (const "") id)
                        <$> SAS.makeJWT
                            (AuthTokenPayload landMemorizerId)
                            jwtSettings
                            Nothing
    signUp langMemorizer = do
        insertedRowsCount <- liftIO $ addLanguageMemorizer dbConn langMemorizer
        if insertedRowsCount == 0
            then S.throwError S.err409
            else pure S.NoContent
    unshareSet (SAS.Authenticated (AuthTokenPayload langMemorizerId)) sharedSetId = do
        sharedSetOwnerId <- liftIO $ getSetOwnerId dbConn sharedSetId
        if sharedSetOwnerId == langMemorizerId
            then liftIO $ deleteSet dbConn sharedSetId >> pure S.NoContent
            else S.throwError S.err403
    unshareSet _ _ = S.throwError S.err401
    updateSharedSet
        (SAS.Authenticated (AuthTokenPayload langMemorizerId))
        (C.SharedSet sharedSetId (C.Set name mdUnits))
        = do
        sharedSetOwnerId <- liftIO $ getSetOwnerId dbConn sharedSetId
        if sharedSetOwnerId == langMemorizerId
            then liftIO . withTransaction dbConn $ do
                deleteSet dbConn sharedSetId
                newSharedSetId <- liftIO $ addSet dbConn langMemorizerId name
                addUnitsWithTrans newSharedSetId mdUnits
                pure $ show newSharedSetId
            else S.throwError S.err403
    updateSharedSet _ _ = S.throwError S.err401

    addUnitsWithTrans :: SharedSetId -> Maybe [C.Unit] -> IO ()
    addUnitsWithTrans sharedSetId (Just units) = mapM_
        (\(C.Unit text translates) -> do
            unitId <- addUnit dbConn sharedSetId text
            void $ mapM_ (addTranslate dbConn unitId) translates)
        units
    addUnitsWithTrans _           Nothing      = pure ()
type SharedSetId = Int
