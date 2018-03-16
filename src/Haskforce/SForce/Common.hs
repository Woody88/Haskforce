{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Haskforce.SForce.Common where

import Data.Text (Text)
import Data.Aeson 
import GHC.Generics

data SObjectAttr = SObjectAttr
    { sobjectType :: SObjectType
    , url         :: UrlPath
    } deriving (Generic, Show)

data SObject a = SObject 
    { attributes :: SObjectAttr 
    , sobject    :: a
    } deriving (Generic, Show)

type UrlPath = Text 

newtype SObjectId = SObjectId Text deriving (Generic, Show)
newtype SObjectType = SObjectType Text deriving (Generic, Show)

--- Instance FromJSON definition for all required types
instance FromJSON SObjectId where
-- TODO: 
instance FromJSON SObjectType where
-- TODO: 
instance FromJSON SObjectAttr where
-- TODO: 
instance FromJSON a => FromJSON (SObject a) where
-- TODO: 
