{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


{- This module will require a major refactoring...-}

module Haskforce.Client 
    ( module Haskforce.Types
    , module Haskforce.API.Resources
    , ClientRequest
    , SFBaseUrl
    , SFApiNumber
    , SFClient(..)
    , NoContent
    , requestAuthentication
    , salesforceUrl
    , localHostTestUrl
    , apiVersion
    , getSObjectRow
    , updateSObjectRow
    , deleteSObjectRow
    , getQuery
    , getNextQuery
    , getSFQueryClient
    , getSFNextQueryClient
    , getSObjectRowClient
    , updateSObjectRowClient
    , deleteSObjectRowClient
    ) 
    where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types as AT
import Data.Proxy 
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Core.Internal.BaseUrl
import Web.Internal.FormUrlEncoded ( ToForm(..) )
import Web.Internal.HttpApiData (toQueryParam)
-- import qualified Data.Yaml             as Yaml
import Control.Exception 
import Haskforce.Types
import Haskforce.API.Oauth
import Haskforce.API.Resources
import Haskforce.SForce.Common
import Haskforce.SForce.Query (Query, SQuery)
import Generics.Eot

class ClientRequest a where
    authenticateQuery :: a -> ClientM AuthResponse

instance ClientRequest TokenRequest where
    authenticateQuery = loginClient

-- This function can throw SevantError Exception - will let enduser handle it the way they want.
requestAuthentication :: ClientRequest a => a -> SFBaseUrl -> IO (Either ServantError AuthResponse)
requestAuthentication request baseUrl = do
  manager' <- newManager tlsManagerSettings
  runClientM (authenticateQuery request) (ClientEnv manager' baseUrl Nothing)

loginClient :: TokenRequest -> ClientM AuthResponse
loginClient tr = token tr

getSFversions :: AccessToken -> ClientM [Version] 
getSFversions accessToken' = versions $ Just accessToken'

apiVersion :: SFClient -> IO [Version] 
apiVersion (SFClient _ sfclient) = do
    manager' <- newManager tlsManagerSettings
    baseUrl  <- getBaseUrl
    res <- runClientM (getSFversions accessToken') (ClientEnv manager' baseUrl Nothing)
    case res of
        Left err -> throwIO err
        Right sfApiVersion -> return (sfApiVersion)
    where accessToken' = accessToken sfclient
          getBaseUrl = liftIO $ parseBaseUrl $ (T.unpack (instanceUrl sfclient)) ++ "/services" 

getSObjectRowClient :: (ToJSON a, FromJSON a, SFObject a) => SFClient -> SObjectId ->  Proxy a ->  ClientM (SObject a)
getSObjectRowClient (SFClient apiv sfclient) sobjId proxy = g access (sobjectName proxy) sobjId Nothing
    where (g :<|> _ :<|> _) =  clientSObjectRow proxy
          access     = Just . accessToken $  sfclient

updateSObjectRowClient :: forall a. (ToJSON a, FromJSON a, SFObject a) => SFClient -> SObjectId -> a -> ClientM NoContent
updateSObjectRowClient (SFClient apiv sfclient) sobjId sobj = p access (sobjectName proxy) sobjId sobj
    where (_ :<|> p :<|> _) =  clientSObjectRow proxy
          proxy      = Proxy :: Proxy a
          access     = Just . accessToken $  sfclient

deleteSObjectRowClient :: forall a. (ToJSON a, FromJSON a, SFObject a) => SFClient -> SObjectId -> Proxy a -> ClientM NoContent
deleteSObjectRowClient (SFClient apiv sfclient) sobjId proxy = d access (sobjectName proxy) sobjId 
    where (_ :<|> _ :<|> d) =  clientSObjectRow proxy
          access     = Just . accessToken $  sfclient

getSFQueryClient :: (ToJSON a, FromJSON a, SFObject a) => SFClient -> Proxy a -> String -> ClientM (Query a)
getSFQueryClient (SFClient apiv sfclient) proxy query = q access (Just query)
    where (q :<|> _)    = clientSFQuery proxy
          access = Just . accessToken $  sfclient

getSFNextQueryClient :: (ToJSON a, FromJSON a, SFObject a) => SFClient -> Proxy a -> Text -> ClientM (Query a)
getSFNextQueryClient (SFClient apiv sfclient) proxy query = q access query
    where (_:<|> q)    = clientSFQuery proxy
          access = Just . accessToken $  sfclient

getQuery :: (FromJSON a, ToJSON a, SFObject a) => String -> Proxy a -> SFClient -> IO (Either ServantError (Query a))
getQuery query proxy sfclient = performRequest sfclient "/query" . getSFQueryClient sfclient proxy $ query

getNextQuery :: (FromJSON a, ToJSON a, SFObject a) => Text -> Proxy a -> SFClient -> IO (Either ServantError (Query a))
getNextQuery url proxy sfclient = performRequest sfclient "/query" . getSFNextQueryClient sfclient proxy $ nextQueryId url
    where nextQueryId = snd . T.breakOnEnd "/"

getSObjectRow :: forall a. (ToJSON a, FromJSON a, SFObject a)  => SObjectId ->  Proxy a -> SFClient -> IO (Either ServantError (SObject a))
getSObjectRow sobjId proxy sfclient = performRequest sfclient "/sobjects" . getSObjectRowClient sfclient sobjId $ proxy

updateSObjectRow :: forall a. (ToJSON a, FromJSON a, SFObject a)  => SObjectId -> a ->  SFClient ->  IO (Either ServantError NoContent)
updateSObjectRow sobjId obj sfclient = performRequest sfclient "/sobjects" . updateSObjectRowClient sfclient sobjId $ obj

deleteSObjectRow :: forall a. (ToJSON a, FromJSON a, SFObject a)  => SObjectId ->  Proxy a -> SFClient ->  IO (Either ServantError NoContent)
deleteSObjectRow sobjId proxy sfclient = performRequest sfclient "/sobjects". deleteSObjectRowClient sfclient sobjId $ proxy

performRequest :: SFClient -> String -> ClientM a -> IO (Either ServantError a)
performRequest (SFClient apiv sfclient) ressourceName client = do
    manager' <- newManager tlsManagerSettings
    baseUrl  <- getBaseUrl
    runClientM client (ClientEnv manager' baseUrl Nothing)
    where getBaseUrl   = liftIO $ 
                         parseBaseUrl $ 
                         (T.unpack (instanceUrl sfclient)) ++ "/services/data/" 
                                                           ++ (T.unpack apiv) 
                                                           ++ ressourceName

localHostTestUrl = (BaseUrl Http "localhost" 8080 "")

salesforceUrl :: BaseUrl 
salesforceUrl = (BaseUrl Https "login.salesforce.com" 443 "/services/oauth2")
    

