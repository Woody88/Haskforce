{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Haskforce.Internal
    ( Resp 
    , HFClient(..)
    , HFCred(..)
    , AccessToken(..)
    , HForceBadConfig (..)
    ) 
    where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import Data.Aeson
import Network.HTTP.Client (Response)
import Control.Exception 

type Resp = Response HFClient

newtype HForceBadConfig = 
    HForceBadConfig String
    deriving (Show)

instance Exception HForceBadConfig

data HFClient = 
    HFClient { accessToken  :: Text
             , refreshToken :: Maybe Text
             , instanceUrl  :: Text 
             , urlId        :: Text  
             , signature    :: Text
             , issueAt      :: Int 
             } deriving (Show, Generic)

data HFCred = 
    HFCred {  grantType        :: Text
            , clientId         :: Text
            , clientSecret     :: Text
            , username         :: Text
            , password         :: Text
            , url              :: Text
            , apiVersion       :: Text
            , redirectCallback :: Maybe Text
            } deriving (Show, Generic)

newtype AccessToken = 
    AccessToken { fromAccess :: HFClient } 
    deriving Show

instance FromJSON HFCred where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = camelTo2 '_'
    }

instance FromJSON HFClient where
    parseJSON (Object v) = HFClient 
        <$> v .: "access_token"
        <*> v .:? "refresh_token" .!= Nothing
        <*> v .: "instance_url"
        <*> v .: "id"
        <*> v .: "signature"
        <*> v .: "issue_at"

instance ToJSON HFCred where
    toJSON c = object 
        [ "client_id"     .= clientId c
        , "client_secret" .= clientSecret c
        , "username"      .= username c
        , "password"      .= password c
        , "grant_type"    .= grantType c
        , "url"           .= url c
        , "api_version"   .= apiVersion c
        ]
        