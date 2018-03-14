{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Haskforce.API.Resources where

import Data.Text
import Data.Proxy
import Data.Aeson (Object)
import Servant.API
import Servant.Client
import Web.HttpApiData
import Haskforce.Types.SForce(Version, SObject, SFId)
import Haskforce.Types.Utils (AccessToken)
import Haskforce.API.Resources.SObjectRow
import Haskforce.API.Resources.Version

type Data = "data" 
type API = Data :> SFVersion


api :: Proxy API
api = Proxy

versions :: Maybe AccessToken -> ClientM [Version]

clientSObjectRow :: HasClient (SObjectRow a) => Proxy a -> Client (SObjectRow a)
clientSObjectRow _ = client (Proxy :: Proxy (SObjectRow a))

(versions) = client api
(getSFObject :<|> postSFObject) = clientSObjectRow (Proxy @ SObject)





