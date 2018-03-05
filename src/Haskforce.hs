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


login :: ClientRequest a => a -> SFBaseUrl -> SFApiNumber -> IO (SFClient)
login = requestAuthentication

sobject ::  SFClient -> Text -> Text -> [Text] -> IO SObject
sobject = getSObject