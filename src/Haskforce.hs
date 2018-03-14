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
import Haskforce.Client as Client
import Data.Proxy
import Data.Aeson 
import Data.Aeson.Types (Parser)
import GHC.Generics
import Generics.Eot

{-| Example. This demonstrate how an end user would define their own SObject Types.
 
data Account = Account
    { id_ :: Text
    , name :: Text
    } deriving (Generic, Show)

instance HFFromJSON Account
instance HFToJSON Account  

-}

-- I.E: login tokenRequest salesforceUrl "v42.0"
login :: ClientRequest a => a -> SFBaseUrl -> SFApiNumber -> IO SFClient
login = requestAuthentication

-- -- I.E: sobject sfclient <SobjectId> (Proxy :: Proxy a)
-- sobject :: (HasEot a, HFFromJSON a) =>  SFClient -> Text -> Proxy a-> IO (Maybe a)
-- sobject = getSObject''

-- -- I.E: sobject sfclient "Account" "<SobjectId>" ["Id", "Name"]
-- sobject' ::  SFClient -> Text -> Text -> [Text] -> IO (SObject)
-- sobject' = getSObject

