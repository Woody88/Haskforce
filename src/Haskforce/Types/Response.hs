{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Haskforce.Types.Response 
    ( AuthResponse(..)
    )
    where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Haskforce.Types.Utils

data AuthResponse = AuthResponse
    { accessToken  :: AccessToken
    , refreshToken :: Maybe RefreshToken
    , expiresIn    :: Maybe Text 
    , instanceUrl  :: Text 
    , urlId        :: Text  
    , signature    :: Text
    , issuedAt     :: Text 
    } deriving (Show, Generic)


instance FromJSON AuthResponse where
    parseJSON = withObject "auth_response" $ \v-> do
        token     <- v .: "access_token"
        refresh   <- v .:? "refresh_token" .!= Nothing
        expires   <- v .:? "expires_in" .!= Nothing
        instanceU <- v .: "instance_url" 
        idU       <- v .: "id" 
        sig       <- v .: "signature" 
        issue     <- v .: "issued_at" 
        return $ (AuthResponse token refresh expires instanceU idU sig issue)
 

-- instance ToJSON AuthReponse where
--     toJSON x = object
--         ["access_token" .= (getAccessToken $ accessToken x)
--         , "instance_url" .= instanceUrl x
--         , "id" .= urlId x
--         , "signature" .= signature x
--         , "issue_at" .= issueAt x
--         ] 