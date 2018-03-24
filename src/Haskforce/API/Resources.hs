{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Haskforce.API.Resources (versions, clientSObjectRow, getSFObject, postSFObject, deleteSFObject) where

import Data.Text
import Data.Proxy
import Data.Aeson (Object, Value)
import Servant.API
import Servant.Client
import Web.HttpApiData
import Haskforce.Types.SForce(Version) --- Need to remove this and add in SForce separate module
import Haskforce.SForce.Common (SObject, SObjectId)
import Haskforce.Types.Utils (AccessToken)
import Haskforce.API.Resources.SObjectRow
import Haskforce.API.Resources.Version

type Data = "data" 
type API = Data :> SFVersion


api :: Proxy API
api = Proxy

-- Salesforce Version endpoint 
versions :: Maybe AccessToken -> ClientM [Version]

-- Salesforce SobjectRow endpoints
clientSObjectRow :: HasClient (SObjectRow a) => Proxy a -> Client (SObjectRow a)
clientSObjectRow _ = client (Proxy :: Proxy (SObjectRow a))

(versions) = client api
(getSFObject :<|> postSFObject :<|> deleteSFObject) = clientSObjectRow (Proxy @ Value)





