{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Haskforce.API.Oauth where

import Haskforce.API.Oauth.Token
import Data.Proxy
import Servant.API
import Servant.Client

type API = OauthTokenAPI

api :: Proxy API
api = Proxy

(token) = client api

