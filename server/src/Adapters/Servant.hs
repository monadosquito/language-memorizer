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

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (), ToJSON ())
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.UTF8 (fromString)
import Data.CaseInsensitive (mk)
import GHC.Generics (Generic ())
import System.Environment (getEnv)

import Common (LanguageMemorizer (), Set (Set), Unit (Unit))
import Ports.Database (DbConnection (..))
import Ports.WwwServer (WwwServer (..))

import qualified Data.ByteString.Char8 as DBC
import qualified Network.Wai.Handler.Warp as NWHW
import qualified Network.Wai.Middleware.Cors as NWMC
import qualified Servant as S
import qualified Servant.Auth.Server as SAS


type Api
    =      "sign-in" S.:> S.ReqBody '[S.JSON] LanguageMemorizer
        S.:> S.Post '[S.JSON] (String, String)
    S.:<|> "sign-up" S.:> S.ReqBody '[S.JSON] LanguageMemorizer
        S.:> S.PostNoContent '[S.PlainText] S.NoContent
    S.:<|> SAS.Auth '[SAS.JWT] AuthTokenPayload
        S.:> "share-set" S.:> S.ReqBody '[S.JSON] Set
            S.:> S.PostNoContent '[S.PlainText] S.NoContent
    S.:<|> S.Verb 'S.OPTIONS 200 '[S.PlainText] S.NoContent

data AuthTokenPayload = AuthTokenPayload
    { _id :: Int
    } deriving anyclass (FromJSON, SAS.FromJWT, SAS.ToJWT, ToJSON)
      deriving stock (Eq, Generic, Show)

data Servant = Servant deriving (Eq, Show)

instance WwwServer Servant where
    run dbCon = do
        allowedReqHeaders <- getEnv "allowed_request_headers"
        allowedReqOrigs <- getEnv "allowed_request_origins"
        jwtKey <- SAS.generateKey
        let
            corsPolicy = NWMC.simpleCorsResourcePolicy
                { NWMC.corsOrigins        =
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
server dbConn jwtSettings = signIn S.:<|> signUp S.:<|> shareSet S.:<|> corsOptions
  where
    corsOptions = pure S.NoContent
    shareSet (SAS.Authenticated _) (Set name mbUnits) = do
        liftIO . withTransaction dbConn $ do
            sharedSetId <- addSet dbConn name
            case mbUnits of
                Just units -> mapM_
                    (\(Unit text translates) -> do
                        unitId <- addUnit dbConn sharedSetId text
                        mapM_ (addTranslate dbConn unitId) translates
                        pure ())
                    units
                Nothing    -> pure ()
            pure ()
        pure S.NoContent
    shareSet _                     _                       = S.throwError S.err401
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
