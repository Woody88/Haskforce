{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Haskforce.MockServer where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Mock
import           Test.QuickCheck.Arbitrary

newtype User = User { username :: String }
    deriving (Eq, Show, Arbitrary, Generic)

instance ToJSON User
instance FromJSON User

type API = "token" :> Get '[JSON] User

api :: Proxy API
api = Proxy

runMock :: IO ()
runMock = run 8080 (serve api $ mock api Proxy)

mkApp :: IO Application
mkApp = return $ (serve api $ mock api Proxy)