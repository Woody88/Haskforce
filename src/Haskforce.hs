{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Haskforce 
    ( login
    , sobject
    ) 
    where

import Data.Text (Text)
import Haskforce.Client

-- I.E: login tokenRequest salesforceUrl "v42.0"
login :: ClientRequest a => a -> SFBaseUrl -> SFApiNumber -> IO SFClient
login = requestAuthentication

-- I.E: sobject sfclient "Account" "<SobjectId>" ["Id", "Name"]
sobject ::  SFClient -> Text -> Text -> [Text] -> IO SObject
sobject = getSObject