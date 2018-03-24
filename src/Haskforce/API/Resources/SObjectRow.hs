{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- Salesforce SobjectRow API reference link 
-- https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/resources_sobject_retrieve.htm
module Haskforce.API.Resources.SObjectRow  where

import Data.Text
import Servant.API
import Data.Aeson
import Haskforce.SForce.Common (SObject, SObjectId, SFErrorResponse, SFSucessNoContent, SFNoContentResponse)
import Haskforce.Types.Utils (AccessToken)


type HeaderAuth    = Header "Authorization" AccessToken
-- type SObjectNameId = Capture "sobjectName" Text :> Capture "id" SObjectId <-- getting error message no instance HasClient
type SObjectRow a = HeaderAuth :> Capture "sobjectName" Text 
                               :> Capture "id" SObjectId
                               :> QueryParam "fields" Text 
                               :> Get  '[JSON] (SObject a)
                :<|> HeaderAuth :> Capture "sobjectName" Text 
                                :> Capture "id" SObjectId 
                                :> ReqBody '[JSON] a 
                                :> Patch '[JSON] NoContent
                :<|> HeaderAuth :> Capture "sobjectName" Text 
                                :> Capture "id" SObjectId 
                                :> Delete '[JSON] NoContent
                    