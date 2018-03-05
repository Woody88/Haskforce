{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Haskforce.API.Resources where

import Data.Text
import Data.Proxy
import Data.Aeson (Object)
import Servant.API
import Servant.Client
import Web.HttpApiData
import Haskforce.Types.SForce(Version, SObject)
import Haskforce.Types.Utils (AccessToken)

type API = "data" :> SFEndpoints

type SFEndpoints = SFVersion :<|> SFObject
type SFVersion = Header "Authorization" AccessToken  :> Get '[JSON] [Version]
type SFObject   = Header "Authorization" AccessToken :> Capture "apiVersion" Text
                                                     :> Capture "resourceName" Text
                                                     :> Capture "sobjectName" Text 
                                                     :> Capture "sobjectId" Text
                                                     :> QueryParam "fields" Text :> Get '[JSON] SObject

api :: Proxy API
api = Proxy

(versions :<|> sobject) = client api

