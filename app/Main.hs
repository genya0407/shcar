module Main where

import           Api (app)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 8080 app
