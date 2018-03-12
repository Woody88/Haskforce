{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

-- Salesforce SobjectRow API reference link 
-- https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/resources_sobject_retrieve.htm
module Haskforce.API.Resources.SObjectRow where

import Data.Text
import Servant.API
import Data.Aeson
import Haskforce.Types.SForce (SObject)
import Haskforce.Types.Utils (AccessToken)

type HeaderAuth = Header "Authorization" AccessToken
type SObjectRow  = HeaderAuth :> Capture "apiVersion" Text
                              :> Capture "resourceName" Text
                              :> Capture "sobjectName" Text 
                              :> Capture "sobjectId" Text
                              :> QueryParam "fields" Text 
                              :> Get '[JSON] SObject

