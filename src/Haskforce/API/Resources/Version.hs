{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

-- Salesforce Version API reference link 
-- https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/resources_versions.htm
module Haskforce.API.Resources.Version where

import Servant.API
import Haskforce.Types.SForce(Version)
import Haskforce.Types.Utils (AccessToken)

type HeaderAuth = Header "Authorization" AccessToken
type SFVersion = HeaderAuth :> Get '[JSON] [Version]