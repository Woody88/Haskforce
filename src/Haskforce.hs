{-# LANGUAGE OverloadedStrings #-}

module Haskforce 
    ( login

    ) 
    where

import Haskforce.Internal 
import Haskforce.Client 
import Control.Exception 


login :: AuthType -> IO (AccessToken, HFClient)
login authtype = authenticateClient authtype

loginWith :: AuthType -> HFCred -> IO (AccessToken, HFClient)
loginWith authtype hfcred = authenticateClientWith authtype hfcred

