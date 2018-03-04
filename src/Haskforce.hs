{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Haskforce 
    ( login
    ) 
    where

import Haskforce.Client


login :: ClientRequest a => a -> SFBaseUrl -> IO (AuthResponse)
login = requestAuthentication 
