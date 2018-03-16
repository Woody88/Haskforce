{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Haskforce.Types.SForce where

import Data.Typeable (typeOf)
import Data.Data as D
import Data.Text (Text)
import           qualified Data.Text as T
import           qualified Data.Char as Char
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser
import GHC.Generics  hiding (datatypeName)
import Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy as HML
import Generics.Eot
import Data.List
import Web.HttpApiData (ToHttpApiData(..))

newtype SFId = SFId Text deriving Show

type SObject = Value

newtype SObject' a = SObject' SObject

class DefaultWithId a where
    defWithId :: SFId -> a
  
data Version = Version
    { version :: Text
    , label   :: Text 
    , url     :: Text
    } deriving (Show, Generic)

instance FromJSON Version

instance FromJSON SFId where
    parseJSON = withText "Id" $ \i -> do
        return $ SFId i

instance ToJSON SFId where
    toJSON (SFId i) = object 
        [ "Id" .= i ]

instance ToHttpApiData SFId where
    toQueryParam (SFId id) = id

adjustFromJsonField :: String -> String
adjustFromJsonField "Id" = "AccountId"
adjustFromJsonField "AccountId" = "Id"
adjustFromJsonField "type_" = "type"
adjustFromJsonField "fields" = "data"
adjustFromJsonField x = x

toLowerCase :: String -> String 
toLowerCase = T.unpack . T.toLower . T.pack

capitalized :: String -> String
capitalized [] = []
capitalized (head:tail) = Char.toUpper head : lowered tail
  where
    lowered [] = []
    lowered (head:tail) = Char.toLower head : lowered tail

deCapitalized :: String -> String
deCapitalized (head:tail) = Char.toLower head : tail

firstCharToUpper :: String -> String 
firstCharToUpper (head:tail) = Char.toUpper head : tail



class (Generic a, GFromJSON Zero (Rep a)) => HFFromJSON a where
    myFromJSON :: Value -> Parser a
    myFromJSON (Object v) = genericParseJSON defaultOptions { fieldLabelModifier = adjustFromJsonField . firstCharToUpper} v'
        where v' = Object (HMS.delete "attributes" $ v) :: Value

class (Typeable a, Generic a, GToJSON Zero (Rep a)) => HFToJSON a where
    myToJSON :: a -> Value
    myToJSON = genericToJSON defaultOptions { fieldLabelModifier = capitalized . adjustFromJsonField}

    keys :: a -> [Text]
    keys = ks' . myToJSON
        where ks' (Object v) = HML.keys v    

    sobjectName :: a -> Text
    sobjectName = T.pack . show . typeOf

class (FromJSON a) => SObjectRow a where 
    parseFromJSON :: Value -> Result a 
    parseFromJSON (Object v) = fromJSON v'
        where v' = Object (HMS.delete "attributes" $ v) :: Value

namesOfFields :: HasEot a => Proxy a -> [String]
namesOfFields proxy =
    nub $
    concatMap (fieldNames . fields) $
    constructors $ datatype proxy
    where
    fieldNames :: Fields -> [String]
    fieldNames fields = case fields of
        Selectors names -> names
        _ -> []


