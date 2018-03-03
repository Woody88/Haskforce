{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

module Haskforce.API.Oauth.Token (OauthTokenAPI) where

import Data.Aeson (toJSON)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS
import Web.Internal.FormUrlEncoded ( ToForm(..), Form(..))
import Web.Internal.HttpApiData (toQueryParam)
import Servant.API
import Servant.Client
import Haskforce.Types 
import Haskforce.Internal (jsonToForm)

type OauthTokenAPI = "token" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] AuthResponse

instance ToForm TokenRequest where
    toForm token = Form $ jsonToForm . toJSON $ token
