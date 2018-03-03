{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-| This module sets up all request based on salesforces numerous
    Authentication Flow. Still cannot figure out if I should
    let users deal with the proper authentication flow on their own
    or provide them support with types...
|-}

module Haskforce.Types.Request 
    ( AuthType(..)
    , GrantType(..)
    , TokenRequest(..)
    ) 
    where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Haskforce.Internal (merge_aeson)
import Haskforce.Types.UserCred
import Haskforce.Types.Utils hiding (UserPassword)
import qualified Haskforce.Types.Utils as U
import Servant.Client

data AuthType =
      UserPassword
    | UserAgent
    | WebServer
    | Resfresh  

data GrantType =
      Password
    | Authorize
    | Revoke
    deriving (Show, Generic)

data AuthorizeRequest = AuthorizeRequest 
    
data TokenRequest = TokenRequest
    { grantType    :: GrantType
    , authType     :: AuthType
    , userCred     :: UserCred
    , redirectUri  :: Maybe RedirectUri
    , code         :: Maybe Code
    , refreshToken :: Maybe RefreshToken
    }

instance ToJSON GrantType where
    toJSON = String . tshow

instance ToJSON TokenRequest where
    toJSON = generateJSONByAuthType 


class AuthRequest a where
    generateJSONByAuthType :: a -> Value

instance AuthRequest TokenRequest where
    generateJSONByAuthType tr@(TokenRequest Password UserPassword _ _ _ _) = userPassJSON tr
    generateJSONByAuthType _ = undefined

userPassJSON :: TokenRequest -> Value
userPassJSON tr = merge_aeson [toJSON $ userCred tr, grantType']
    where grantType' = object ["grant_type" .= grantType tr] 


tshow :: Show a => a -> Text
tshow = T.toLower . T.pack . show
