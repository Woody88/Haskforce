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
    , requestAuthentication
    , salesforceUrl
    , localHostTestUrl
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
import Haskforce.Types
import Haskforce.API.Oauth


type SFBaseUrl = BaseUrl

class ClientRequest a where
    authenticateQuery :: a -> ClientM AuthResponse

instance ClientRequest TokenRequest where
    authenticateQuery = loginQuery

-- This function can throw SevantError Exception - will let enduser handle it the way they want.
requestAuthentication :: ClientRequest a => a -> SFBaseUrl -> IO (AuthResponse)
requestAuthentication request baseUrl = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (authenticateQuery request) (ClientEnv manager' baseUrl)
  case res of
    Left err -> throwIO err
    Right client -> return (client)

loginQuery :: TokenRequest -> ClientM AuthResponse
loginQuery tr = do
    client <- token tr
    return client
    

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