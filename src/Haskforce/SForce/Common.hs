{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskforce.SForce.Common where

import Data.Proxy
import Data.Text (Text)
import Data.Aeson 
import Data.Aeson.Types
import GHC.Generics
import Servant.Common.BaseUrl
import Data.HashMap.Strict as HMS
import Web.HttpApiData (ToHttpApiData(..))
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace
import Haskforce.Types.Response (AuthResponse)
type UrlPath = Text 


type SFApiNumber = Text
type SFBaseUrl = BaseUrl
type SFNoContentResponse = Either SFErrorResponse SFSucessNoContent

data SFClient = SFClient SFApiNumber AuthResponse deriving Show -- Need to review, making this a newtype will maybe bring more benifits question is how??
data SFSucessNoContent = SucessNoContent deriving (Generic, Show)
data SFErrorResponse = SFError
    { fields    :: Maybe [Text]
    , message   :: Text
    , errorCode :: Text
    } deriving (Generic, Show)

data SObjectAttr = SObjectAttr
    { sobjectType :: SObjectType
    , sobjetUrl   :: UrlPath
    } deriving (Generic, Show)

data SObjectData a = SObjectData 
    { attributes :: SObjectAttr 
    , sobject    :: a
    } deriving (Generic, Show)

newtype SObject a = SObject (SObjectId, (SObjectData a)) deriving (Generic, Show)
newtype SObjectId = SObjectId Text  deriving (Generic, Show)
newtype SObjectType = SObjectType Text deriving (Generic, Show)

{-| data Account = Account 
        { accountId :: SObjectId 
        , name      :: Text
        }

    λ> let x = "{\"attributes\":{\"type\":\"Customerx\",\"url\":\"randoom\"},\"name\":\"Argentina\",\"Id\":\"x01D0000000002RIAQ\"}" :: Data.ByteString.Lazy.Internal.ByteStringlet x = "{\"attributes\":{\"type\":\"Customerx\",\"url\":\"randoom\"},\"name\":\"Argentina\",\"Id\":\"x01D0000000002RIAQ\"}" :: Data.ByteString.Lazy.Internal.ByteString
    λ> eitherDecode x :: Either String (SObject Account)
    Right (SObject {attributes = SObjectAttr {sobjectType = SObjectType "Customerx", url = "randoom"}, sobject = Account {accountId = SObjectId "x01D0000000002RIAQ", name = "Argentina"}})
|-}


--- Returns the text id for servant endpoint param
instance ToHttpApiData SObjectId where
    toQueryParam (SObjectId id) = id

--- Instance ToJSON definition for all required types
instance ToJSON SObjectId where
    toJSON (SObjectId id) = object
        ["Id" .=  id]


--- Instance FromJSON definition for all required types
instance FromJSON SFSucessNoContent
instance FromJSON SFErrorResponse 
instance FromJSON SObjectId where
    parseJSON (Object v) = SObjectId 
        <$> v .: "Id"

instance FromJSON SObjectType where
    parseJSON = withObject "attributes" $ \v -> do
        obj <- v .: "type"
        return $ SObjectType obj 

instance ToJSON SObjectType where
    toJSON (SObjectType x) = object
        ["type" .= x]

instance FromJSON SObjectAttr where
    parseJSON (Object o) = do
        type_ <- (parseJSON $ (Object o)) :: Parser SObjectType
        url_  <- o .: "url"
        return $ SObjectAttr type_ url_

instance ToJSON SObjectAttr where
    toJSON = genericToJSON defaultOptions

--- Since salesforce return a json attributes object
--- We must execute an operation that extracts the attributes object 
--- and parse it using the SObjectAttr FromJSON's parseJSON function
instance FromJSON a => FromJSON (SObjectData a) where
    parseJSON x@(Object v) = SObjectData
        <$> parseJSON attrJSON
        <*> parseJSON sobjectJSON
        where (Just attrJSON) = HMS.lookup "attributes" v
              sobjectJSON  = Object (HMS.delete "attributes" v) :: Value

instance forall a. FromJSON a => FromJSON (SObject a) where
    parseJSON v = do
        si <- parseJSON v :: Parser SObjectId
        sd <- parseJSON v :: Parser (SObjectData a)
        return $ SObject (si,sd)

class SFObject a where
    sobjectName :: Proxy a -> Text 

--- Some helper function for the SObject type

getId :: SObject a -> SObjectId
getId (SObject a) = fst a

getSData :: SObject a -> SObjectData a
getSData (SObject a) = snd a


