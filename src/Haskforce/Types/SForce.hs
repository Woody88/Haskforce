{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Haskforce.Types.SForce where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.HashMap.Lazy as HML

data Version = Version
    { version :: Text
    , label   :: Text 
    , url     :: Text
    } deriving (Show, Generic)

instance FromJSON Version

data SObject = SObject
    { attributes :: SObjectAttr
    , data_      :: HML.HashMap Text Value
    } deriving (Generic, Show)

data SObjectAttr = SObjectAttr
    { type_ :: Text
    , url   :: Text
    } deriving (Generic, Show)

instance FromJSON SObjectAttr where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = adjustFromJsonField
    }

instance FromJSON SObject where
    parseJSON = withObject "sobject" $ \v -> do
        attr <- v .: "attributes"
        let (Just a') = (flip parseMaybe attr $ (\o -> do return o)) :: Maybe SObjectAttr
            fields = HML.delete "attributes" v
        return $ SObject a' fields

adjustFromJsonField "type_" = "type"
adjustFromJsonField "data_" = "data"
adjustFromJsonField x = x


