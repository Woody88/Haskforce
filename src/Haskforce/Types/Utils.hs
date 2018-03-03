{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-| This module containes all the types that needs to be shared across
    the Haskforce.Types folder. However I dont think that this is a correct
    naming nor whether its the correct approach.. Will need to consult some 
    of the more knowledgeable haskellers..
|-}

module Haskforce.Types.Utils where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson 

newtype Username     = Username { user :: Text} deriving Show
newtype UserPassword = Userpassword { pass :: Text } deriving Show
newtype SecretKey    = SecretKey { secret :: Text } deriving Show
newtype RedirectUri  = RedirectUri Text
newtype Code         = Code Text
newtype RefreshToken = RefreshToken { getRefreshToken :: Text } deriving (Show, Generic)
newtype AccessToken  = AccessToken {getAccessToken :: Text } deriving (Show, Generic)

instance FromJSON RefreshToken where
    parseJSON = withText "access_token" $ \r -> do
        return $ RefreshToken r

instance ToJSON RefreshToken where
    toJSON token = object 
        [ "refresh_token" .= getRefreshToken token ]

instance FromJSON AccessToken where
    parseJSON = withText "access_token" $ \t -> do
        return $ AccessToken t

instance ToJSON AccessToken where
    toJSON token = object 
        [ "access_token" .= getAccessToken token ]