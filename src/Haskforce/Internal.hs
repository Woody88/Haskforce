{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-| Ignore this module types for now, they will be removed later.
    only functions that still being used by other modules...
|-}

module Haskforce.Internal
    ( mergeAesonObject
    , valueToText
    , jsonToForm
    ) 
    where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HMS
import Data.Aeson

-- This function can throw an exception if value provided are not aeson Object types.
-- it will filter aeson Null types.  
mergeAesonObject :: [Value] -> Value
mergeAesonObject = Object . HML.unions . map (\(Object x) -> x) . filterNull
    where filterNull = filter (/=Null)

jsonToForm :: Value -> HMS.HashMap Text [Text]
jsonToForm (Object v) =  HMS.map valueToText v

valueToText :: Value -> [Text]
valueToText (String x) = [x]
valueToText (Number x)    = [T.pack $ show x]