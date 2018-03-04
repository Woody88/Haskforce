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
    , GrantTypeOpt (..)
    , TokenRequest(..)
    ) 
    where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import Haskforce.Internal (mergeAesonObject)
import Haskforce.Types.UserCred
import Haskforce.Types.Utils hiding (UserPassword)
import qualified Haskforce.Types.Utils as U
import Servant.Client

type Optionals = HML.HashMap Text Text

data AuthType =
      UserPassword
    | UserAgent
    | WebServer
    | Resfresh  

data GrantTypeOpt =
      Password
    | Authorize
    | Revoke
    deriving (Show, Generic)

newtype GrantType = GrantType {grant_type :: GrantTypeOpt} deriving Generic



data AuthorizeRequest = AuthorizeRequest 
    
data TokenRequest = TokenRequest
    { grantType    :: GrantType
    , authType     :: AuthType
    , userCred     :: UserCred
    , redirectUri  :: Maybe RedirectUri
    , code         :: Maybe Code
    , refreshToken :: Maybe RefreshToken
    , optionals    :: Maybe Optionals
    }

instance ToJSON GrantTypeOpt where
    toJSON = String . tshow

instance ToJSON GrantType

instance ToJSON TokenRequest where
    toJSON = generateJSONByAuthType 

class AuthRequest a where
    generateJSONByAuthType :: a -> Value

instance AuthRequest TokenRequest where
    generateJSONByAuthType tr@(TokenRequest (GrantType Password) UserPassword _ _ _ _ _) = userPassJson tr
    generateJSONByAuthType _ = undefined

userPassJson :: TokenRequest -> Value
userPassJson tr = mergeAesonObject [toJSON $ userCred tr, toJSON $ optionals tr, toJSON $ grantType tr]

tshow :: Show a => a -> Text
tshow = T.toLower . T.pack . show
