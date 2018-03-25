{-# LANGUAGE DeriveGeneric #-}

module Haskforce.SForce.Query where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Web.HttpApiData (ToHttpApiData(..))
import Haskforce.SForce.Common (SObjectAttr, SObject)

newtype SQuery = SQuery Text deriving (Generic, Show)

data Query a = Query 
    { totalSize      :: Int
    , done           :: Bool
    , records        :: [SObject a]
    , nextRecordsUrl :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON a => FromJSON (Query a)

instance ToHttpApiData SQuery where
    toQueryParam (SQuery t) = t

class SObjectQuery a where
-- TODO: 