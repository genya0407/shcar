{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Api (app) where

import           Servant
import qualified Api.Users as Users

type API = "users" :> Users.API

api :: Proxy API
api = Proxy

server :: Server API
server = Users.server

app = serve api server
