{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Haskforce.Client 
    (   login, 
        HFClient(..)
     ) where

import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as S8
import Network.Wreq  
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import qualified Data.Yaml             as Yaml
import Control.Lens

data HFCred = 
    HFCred { grantType        :: Text
            , clientId         :: Text
            , clientSecret     :: Text
            , username         :: Text
            , password         :: Text
            , redirectCallback :: Maybe Text
            , url              :: Text
            , apiVersion       :: Text
            } deriving (Show, Generic)
                        
data HFClient = 
    HFClient { accessToken  :: Text
             , refreshToken :: Text
             , instanceUrl  :: Text 
             , urlId        :: Text  
             , signature    :: Text
             , issueAt      :: Int 
             } deriving (Show, Generic)

instance FromJSON HFCred where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = camelTo2 '_' 
    }

instance FromJSON HFClient where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = urlId_noprefix . camelTo2 '_' 
    }

instance ToJSON HFCred where
    toJSON c = object 
        [ "client_id"     .= _clientId c
        , "client_secret" .= _clientSecret c
        , "username"      .= _username c
        , "password"      .= _password c
        , "grant_type"    .= _granType c
        ]
        
-- Will need this
-- instance ToForm ClientAuth where
--     toForm c = 
--         [ "client_id"     := _clientId c
--         , "client_secret" := _clientSecret c
--         , "username"      := _username c
--         , "password"      := _password c
--         , "grant_type"    := _granType c
--         ]

urlId_noprefix "urlId" = "id" 

parseHForceConfig :: FilePath -> IO (Either String HFCred)
parseHForceConfig file = do
    content <- S8.readFile file
    return . hforceErrorHandler $ (Yaml.decode content :: Maybe HFCred)

hforceErrorHandler :: Maybe HFCred -> Either String HFCred
hforceErrorHandler parsedContent = 
    case parsedContent of
        Nothing -> Left $ "Could not parse config file."
        (Just hfCred) -> Right hfCred


hforceRequestToken :: HFCred -> IO (Either String HFCred) 
hforceRequestToken hfCred = undefined

buildTokenRequest :: HFCred -> StdMethod -> RequestHeaders -> IO (Either String HFClient)
buildTokenRequest hfCred method headers = do
    let request = requestJsonBody $ requestHeaders $ requestUrl
    response <- httpJSON request  -- still needs work unfinished
    -- (getResponseStatusCode response)
    -- (getResponseBody response :: Value)
    where requestHeaders  = setRequestHeaders headers
          requestJsonBody = setRequestBodyJson hfCred
          requestUrl      = parseRequest_ $ T.unpack $ (renderStdMethod method <> " " <> url) 

login :: IO (Either String HFCred)
login = undefined
