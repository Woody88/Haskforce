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
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HML
import qualified Data.ByteString.Lazy.Char8 as LBC8
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client (Response)
import Control.Exception 

type Resp = Response HFClient

newtype HForceBadConfig = 
    HForceBadConfig String
    deriving (Show)

instance Exception HForceBadConfig

type HFCredOptions = Map.Map HFCredOption Text

data HFCredOption
    = Display
    | Scope
    | State
    | Format 
    | CodeVerifier
    | ClientAssertionType
    | RedirectUri
    | Code
    deriving (Show, Generic, Ord, Eq)

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
            , optionals        :: Maybe HFCredOptions
            } deriving (Show, Generic)

data HFCredAuth = 
    HFCredAuth { responseType :: Text
               , client_id    :: Text
               , redirect_uri :: Text
               }


newtype AccessToken = 
    AccessToken { fromAccess :: Text } 
    deriving Show

instance FromJSON HFCred where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = camelTo2 '_'
    }
instance FromJSON HFCredOption 

instance FromJSONKey HFCredOption where
    fromJSONKey = FromJSONKeyText formatTextToType

instance ToJSONKey HFCredOption where
    toJSONKey = toJSONKeyText formatTypeToText

instance ToJSON HFCredOption where
    toJSON = String . formatTypeToText

instance FromJSON HFClient where
    parseJSON (Object v) = HFClient 
        <$> v .: "access_token"
        <*> v .:? "refresh_token" .!= Nothing
        <*> v .: "instance_url"
        <*> v .: "id"
        <*> v .: "signature"
        <*> v .: "issue_at"

instance ToJSON HFCred where
    toJSON c = merge_aeson [requiredFields, toJSON $ optionals c]
        where requiredFields = object 
                                [ "client_id"     .= clientId c
                                , "client_secret" .= clientSecret c
                                , "username"      .= username c
                                , "password"      .= password c
                                , "grant_type"    .= grantType c
                                ]

formatTypeToText :: Show a => a -> T.Text
formatTypeToText = T.pack . (camelTo2 '_') . show

formatTextToType :: T.Text -> HFCredOption
formatTextToType key 
    | key == "display" = Display 
    | otherwise = error "false"

merge_aeson :: [Value] -> Value
merge_aeson = Object . HML.unions . map (\(Object x) -> x)