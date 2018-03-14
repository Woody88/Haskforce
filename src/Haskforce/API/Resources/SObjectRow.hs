{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- Salesforce SobjectRow API reference link 
-- https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/resources_sobject_retrieve.htm
module Haskforce.API.Resources.SObjectRow (SObjectRow(..)) where

import Data.Text
import Servant.API
import Data.Aeson
import Haskforce.Types.SForce (SObject, SFId)
import Haskforce.Types.Utils (AccessToken)

type Data = "data" 
type HeaderAuth = Header "Authorization" AccessToken
type SObjectRow a = HeaderAuth :> Capture "sobjectName" Text 
                               :> Capture "id" SFId
                               :> QueryParam "fields" Text 
                               :> Get  '[JSON] a
                :<|> ReqBody '[JSON]  a :> Post '[JSON] NoContent

-- type SObjectRow  = HeaderAuth :> Capture "apiVersion" Text
--                               :> Capture "resourceName" Text
--                               :> Capture "sobjectName" Text 
--                               :> Capture "sobjectId" Text
--                               :> QueryParam "fields" Text 
--                               :> Get '[JSON] SObject
