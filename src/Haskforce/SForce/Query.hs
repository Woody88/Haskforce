module Haskforce.SForce.Query where

import Haskforce.SForce.Common (SObjectAttr, SObject)

data Query a = Query 
    { totalSize :: Int
    , done      :: Bool
    , records   :: [SObject a]
    }


class SObjectQuery a where
-- TODO: 