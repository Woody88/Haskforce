{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}  
{-# LANGUAGE MonoLocalBinds #-}

module Haskforce 
    ( login
    , module Client 
    , module SFTypes
    , Query(..)
    , SQuery(..)
    ) 
    where

import Data.Text (Text)
import qualified Data.Text as T
import Servant.API
import Servant.Client
import Haskforce.Client as Client
import Haskforce.SForce.Common as SFTypes
import Haskforce.SForce.Query 
import Data.Proxy
import Data.Aeson 
import Data.Aeson.Types 
import GHC.Generics
import Generics.Eot

{-| Example. This demonstrate how an end user would define their own SObject Types.
 
  data Account = Account 
          { name      :: Text
          } deriving (Generic, Show)
      
  instance SFObject Account where
      sobjectName _ = "Account"

  instance ToJSON Account 
  instance FromJSON Account 
-}

-- I.E: login tokenRequest salesforceUrl "v42.0"
login :: ClientRequest a => a -> SFBaseUrl -> SFApiNumber -> IO (Either ServantError SFClient)
login clientRequest sfurl apiv = do
    res <- requestAuthentication clientRequest sfurl 
    validate res
    where validate res = case res of
                            Left x -> return $ Left x
                            Right sfclient -> return $ Right (SFClient apiv sfclient)

