module Main where

import Servant
--import 
import qualified Network.Wai.Handler.Warp as Warp

type UsersApi = "users" :> Get '[JSON] [Todo]

api :: Proxy API
api = Proxy

main :: IO ()
main = someFunc
