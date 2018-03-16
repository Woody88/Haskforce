{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}  
{-# LANGUAGE MonoLocalBinds #-}

module Haskforce 
    ( login
    , module Client 
    ) 
    where

import Data.Text (Text)
import qualified Data.Text as T
import Haskforce.Client as Client
import Data.Proxy
import Data.Aeson 
import Data.Aeson.Types 
import GHC.Generics
import Generics.Eot

{-| Example. This demonstrate how an end user would define their own SObject Types.
 
data Account = Account
    { accountId :: SFId
    , name :: Text
    } deriving (Generic, Show)
    

    instance HFFromJSON Account
    instance HFToJSON Account

    instance FromJSON Account where
      parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = adjustFromJsonField }  
    
    instance ToJSON Account where
      toJSON = genericToJSON defaultOptions { fieldLabelModifier = capitalized . adjustFromJsonField}

-}

-- I.E: login tokenRequest salesforceUrl "v42.0"
login :: ClientRequest a => a -> SFBaseUrl -> SFApiNumber -> IO SFClient
login = requestAuthentication

