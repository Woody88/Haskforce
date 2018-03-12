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
import Haskforce.API.Resources.SObjectRow
import Haskforce.API.Resources.Version

type API = "data" :> SFEndpoints

type SFEndpoints = SFVersion :<|> SObjectRow

api :: Proxy API
api = Proxy

versions :: Maybe AccessToken -> ClientM [Version]
--sobject :: Maybe AccessToken -> Text -> Text -> Text -> Text -> Maybe Text -> ClientM (SObject2 a)

(versions :<|> sobject) = client api




