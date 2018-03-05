{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

{- This module will require a major refactoring...-}

module Haskforce.Client 
    ( module Haskforce.Types
    , ClientRequest
    , SFBaseUrl
    , SFApiNumber
    , SFClient
    , requestAuthentication
    , salesforceUrl
    , localHostTestUrl
    , apiVersion
    , getSObject
    ) 
    where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy 
import Data.Text
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Common.BaseUrl
import Web.Internal.FormUrlEncoded ( ToForm(..) )
import Web.Internal.HttpApiData (toQueryParam)
import qualified Data.Yaml             as Yaml
import Control.Exception 
import Haskforce.Types
import Haskforce.API.Oauth
import Haskforce.API.Resources


data SFClient = SFClient SFApiNumber AuthResponse deriving Show
type SFApiNumber = Text
type SFBaseUrl = BaseUrl

class ClientRequest a where
    authenticateQuery :: a -> ClientM AuthResponse

instance ClientRequest TokenRequest where
    authenticateQuery = loginQuery

-- This function can throw SevantError Exception - will let enduser handle it the way they want.
requestAuthentication :: ClientRequest a => a -> SFBaseUrl -> SFApiNumber -> IO (SFClient)
requestAuthentication request baseUrl apiv = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (authenticateQuery request) (ClientEnv manager' baseUrl)
  case res of
    Left err -> throwIO err
    Right sfclient -> return (SFClient apiv sfclient)

loginQuery :: TokenRequest -> ClientM AuthResponse
loginQuery tr = token tr

getSFversions :: AccessToken -> ClientM [Version] 
getSFversions accessToken' = versions $ Just accessToken'

getSFObject :: AccessToken -> Text -> Text -> Text -> [Text] -> ClientM SObject
getSFObject accessToken' apiv sobj id_ flds = sobject (Just accessToken') apiv resourceName sobj id_ (Just $ T.intercalate "," flds)
    where resourceName = "sobjects"

apiVersion :: SFClient -> IO [Version] 
apiVersion (SFClient _ sfclient) = do
    manager' <- newManager tlsManagerSettings
    baseUrl  <- getBaseUrl
    res <- runClientM (getSFversions accessToken') (ClientEnv manager' baseUrl)
    case res of
        Left err -> throwIO err
        Right sfApiVersion -> return (sfApiVersion)
    where accessToken' = accessToken sfclient
          getBaseUrl = liftIO $ parseBaseUrl $ (T.unpack (instanceUrl sfclient)) ++ "/services"

getSObject :: SFClient -> Text -> Text -> [Text] -> IO SObject
getSObject (SFClient apiv sfclient) sobj id_ flds = do
    manager' <- newManager tlsManagerSettings
    baseUrl  <- getBaseUrl
    res <- runClientM (getSFObject accessToken' apiv sobj id_ flds) (ClientEnv manager' baseUrl)
    case res of
        Left err -> throwIO err
        Right sobj' -> return (sobj')
    where accessToken' = accessToken sfclient
          getBaseUrl = liftIO $ parseBaseUrl $ (T.unpack (instanceUrl sfclient)) ++ "/services"



-- parseHForceConfig :: FilePath -> IO (Either String HFCred)
-- parseHForceConfig file = do
--     content <- S8.readFile file
--     return . hforceErrorHandler $ (Yaml.decode content :: Maybe HFCred)

-- hforceErrorHandler :: Maybe HFCred -> Either String HFCred
-- hforceErrorHandler parsedContent = 
--     case parsedContent of
--         Nothing -> Left $ "Could not parse config file."
--         (Just hfCred) -> Right hfCred

-- authenticateClient :: AuthRequest a => a -> IO (AuthResponse)
-- authenticateClient TokenRequest

-- authFlow Aut


-- getHFCredConfig :: IO HFCred
-- getHFCredConfig = do
--     eitherHfcred <- liftIO $ parseHForceConfig "hforce.yml"
--     handle eitherHfcred
--     where handle :: Either String HFCred -> IO (HFCred)
--           handle (Right cred) = return cred
--           handle (Left x) = throwIO $ HForceBadConfig x

-- authenticateClientWithToken :: HFCred -> IO (AccessToken, HFClient)
-- authenticateClientWithToken hfcred = do
--   manager' <- newManager tlsManagerSettings
--   res <- runClientM (loginQuery hfcred) (ClientEnv manager' testSalesforce)
--   case res of
--     Left err -> throwIO err
--     Right client -> return (AccessToken $ accessToken client, client)

-- authenticateClient :: AuthType -> IO (AccessToken, HFClient)
-- authenticateClient Token = do
--     hfcred <- getHFCredConfig
--     authenticateClientWithToken hfcred
--     where authenticateClient' Token = authenticateClientWithToken
--           authenticateClient' _ = error "Need to implement"


-- authenticateClientWith :: AuthType -> HFCred -> IO (AccessToken, HFClient)
-- authenticateClientWith Token hfcred = authenticateClientWithToken hfcred
-- authenticateClientWith _ _ = error "Need to implement"

localHostTestUrl = (BaseUrl Http "localhost" 8080 "")

salesforceUrl :: BaseUrl 
salesforceUrl = (BaseUrl Https "login.salesforce.com" 443 "/services/oauth2")

-- salesforceResource :: Maybe SFApiNumber -> BaseUrl
-- salesforceResource Nothing = (BaseUrl Https "login.salesforce.com" 443 "/services/data")
-- salesforceResource (Just sfapi) = (BaseUrl Https "" 443 $ "/services/data" ++ sfapi)
