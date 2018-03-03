{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-| This module contains all data type pertinent to
    salesforce user credentials before perform any rest api 
    request.
|-}

module Haskforce.Types.UserCred where

import GHC.Generics
import Data.Aeson 
import Data.Text (Text)
import Data.Monoid ((<>))
import Haskforce.Types.Utils

data UserCred = UserCred
    { username     :: Maybe Username
    , password     :: Maybe UserPassword
    , secretKey    :: Maybe SecretKey
    , clientId     :: Text
    , clientSecret :: Text
    } deriving (Show, Generic)



-- toJSON also makes sure to concat secret key with password as per salesforce conditions.
instance ToJSON UserCred where
    toJSON u@(UserCred Nothing Nothing Nothing _ _) = object
        [ "client_id" .= clientId u
        , "client_secret" .= clientSecret u
        ]
    toJSON u = object 
        [ "client_id" .= clientId u
        , "client_secret" .= clientSecret u
        , "username" .= (getCredVal user $ username  u)
        , "password" .= passwordWithSecret
        ]
        where passwordWithSecret = (getCredVal pass $ password u) <> (getCredVal secret $ secretKey u)
              getCredVal f (Just x) = f x 