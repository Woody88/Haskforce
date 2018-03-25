{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Haskforce.API.Resources.SFQuery where

import Data.Text
import Servant.API
import Data.Aeson
import Haskforce.SForce.Query
import Haskforce.Types.Utils (AccessToken)


type HeaderAuth = Header "Authorization" AccessToken

type SFQuery a =  HeaderAuth :> QueryParam "q" String
                             :> Get '[JSON] (Query a)
             :<|> HeaderAuth :> Capture "nextRecordUrl" Text
                             :> Get '[JSON] (Query a)