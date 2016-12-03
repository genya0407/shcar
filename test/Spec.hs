{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Api (app)
import           Test.Hspec
import           Test.Hspec.Wai hiding (post, get, put, delete)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

commonHeaders = [("Content-Type", "application/json")]
post path body = request "POST" path commonHeaders body
get path = request "GET" path commonHeaders ""
put path body = request "PUT" path commonHeaders body
delete path = request "DELETE" path commonHeaders ""

spec :: Spec
spec = with (return app) $ do
  describe "UsersAPI" $ do
    let user1JSON = [json|
                     {
                       "userName": "Yusuke Sangenya",
                       "userMail": "example1@example.com",
                       "userPhoneNumber":"0123456789"
                     }
                   |]
        user2JSON = [json|
                     {
                       "userName": "Yusuke Sangenya",
                       "userMail": "example2@example.com",
                       "userPhoneNumber":"0123456789"
                     }
                   |]

    it "Create user" $ do
      post "/users" user1JSON `shouldRespondWith` 200
      post "/users" user2JSON `shouldRespondWith` 200

    it "Get users" $ do
      get "/users" `shouldRespondWith` [json|
           [{
             "userName": "Yusuke Sangenya",
             "userMail": "example1@example.com",
             "userPhoneNumber":"0123456789"
           },
           {
             "userName": "Yusuke Sangenya",
             "userMail": "example2@example.com",
             "userPhoneNumber":"0123456789"
           }]
         |]
    
    it "Get user" $ do
      get "/users/1" `shouldRespondWith` [json|
           {
             "userName": "Yusuke Sangenya",
             "userMail": "example1@example.com",
             "userPhoneNumber":"0123456789"
           }
         |]

    it "Delete user" $ do
      delete "/users/2" `shouldRespondWith` 200
      get "/users/2" `shouldRespondWith` 404

    it "Update user" $ do
      put "/users/1" user2JSON `shouldRespondWith` 200
      get "/users/1" `shouldRespondWith` [json|
          {
            "userName": "Yusuke Sangenya",
            "userMail": "example2@example.com",
            "userPhoneNumber": "0123456789"
          }
        |]

