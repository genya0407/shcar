{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant
import Servant.Mock
import Model
import qualified Network.Wai.Handler.Warp as Warp

type UsersAPI = "users" :> Get '[JSON] [User]

userApi :: Proxy UsersAPI
userApi = Proxy

main :: IO ()
main = Warp.run 8080 $ serve userApi (mock userApi mempty)
