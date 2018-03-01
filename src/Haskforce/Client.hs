{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

module Haskforce.Client
    ( AuthType(..) 
    , authenticateClient
    , authenticateClientWith
    ) 
    where

import Control.Monad.IO.Class
import Haskforce.Internal
import Data.Aeson
import Data.Proxy 
import Data.Text
import qualified Data.ByteString.Char8 as S8
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS
import Servant.API
import Web.Internal.FormUrlEncoded ( ToForm(..) )
import Web.Internal.HttpApiData (toQueryParam)
import Servant.Client
import qualified Data.Yaml             as Yaml
import Control.Exception 

data AuthType =
      Token
    | Authorize
    | Revoke

(<>) :: Text -> Text -> (Text,Text)
a <> b = (a, b)

instance ToForm HFCred where
    toForm c =
        [ "client_id"     <> (toQueryParam $ clientId c)
        , "client_secret" <> (toQueryParam $ clientSecret c)
        , "username"      <> (toQueryParam $ username c)
        , "password"      <> (toQueryParam $ password c)
        , "grant_type"    <> (toQueryParam $ grantType c)
        ]

type AuthAPI =   "token" :> ReqBody '[FormUrlEncoded] HFCred :> Post '[JSON] HFClient
            :<|> "salesforce" :> ReqBody '[JSON, FormUrlEncoded] HFCred :> Post '[JSON] HFClient
            :<|> EmptyAPI

parseHForceConfig :: FilePath -> IO (Either String HFCred)
parseHForceConfig file = do
    content <- S8.readFile file
    return . hforceErrorHandler $ (Yaml.decode content :: Maybe HFCred)

hforceErrorHandler :: Maybe HFCred -> Either String HFCred
hforceErrorHandler parsedContent = 
    case parsedContent of
        Nothing -> Left $ "Could not parse config file."
        (Just hfCred) -> Right hfCred

api :: Proxy AuthAPI
api = Proxy

(token :<|> salesforce :<|> EmptyClient) = client api


loginQuery :: HFCred -> ClientM HFClient
loginQuery hfcred = do
    client <- salesforce hfcred
    return client


getHFCredConfig :: IO HFCred
getHFCredConfig = do
    eitherHfcred <- liftIO $ parseHForceConfig "hforce.yml"
    handle eitherHfcred
    where handle :: Either String HFCred -> IO (HFCred)
          handle (Right cred) = return cred
          handle (Left x) = throwIO $ HForceBadConfig x

authenticateClientWithToken :: HFCred -> IO (AccessToken, HFClient)
authenticateClientWithToken hfcred = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (loginQuery hfcred) (ClientEnv manager' testSalesforce)
  case res of
    Left err -> throwIO err
    Right client -> return (AccessToken $ accessToken client, client)

authenticateClient :: AuthType -> IO (AccessToken, HFClient)
authenticateClient Token = do
    hfcred <- getHFCredConfig
    authenticateClientWithToken hfcred
    where authenticateClient' Token = authenticateClientWithToken
          authenticateClient' _ = error "Need to implement"


authenticateClientWith :: AuthType -> HFCred -> IO (AccessToken, HFClient)
authenticateClientWith Token hfcred = authenticateClientWithToken hfcred
authenticateClientWith _ _ = error "Need to implement"

testSalesforce = (BaseUrl Http "localhost" 8080 "")

salesforceUrl :: BaseUrl 
salesforceUrl = (BaseUrl Https "login.salesforce.com" 443 "/services/oauth2")