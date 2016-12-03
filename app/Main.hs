{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}

module Main where

import           Servant
import           Servant.Mock
import           Model
import           Control.Monad.IO.Class
import qualified Network.Wai.Handler.Warp as Warp
import           Database.Persist
import           Database.Persist.Sqlite
import           Control.Arrow (left, right)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy (fromStrict)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Monoid ((<>))

type UsersAPI = Get '[JSON] [User]
           :<|> ReqBody '[JSON] User :> Post '[JSON] User
           :<|> Capture "id" Int :> Get '[JSON] User
           :<|> Capture "id" Int :> ReqBody '[JSON] User :> Put '[JSON] ()
           :<|> Capture "id" Int :> Delete '[JSON] ()

userApi :: Proxy UsersAPI
userApi = Proxy

server :: Server UsersAPI
server = index :<|> create :<|> read' :<|> update :<|> delete'
  where
    index = pure mempty
    create user = do
      insertedUser <- runDB $ insertUnique user
      case insertedUser of
        Just _ -> pure user
        Nothing -> throwError $ emailUsed (userMail user)
    read' _id = do
      gotUser <- runDB $ get (toSqlKey $ fromIntegral _id)
      case gotUser of
        Just user -> pure user
        Nothing -> throwError $ err404 { errBody = "user with id = " <> (pack . show) _id <> "doesn't exist." }
    update _id user = afterGet _id $ \key -> do
      runDB $ repsert key user
      pure mempty
    delete' _id = afterGet _id $ \key -> do
      runDB $ delete key
      pure mempty

err422 = ServantErr { errHTTPCode = 422, errBody = "", errReasonPhrase = "Unprocessable Entity", errHeaders = [] }
emailUsed mail = err422 { errBody = "email address \"" <> (encodeUtf8 $ fromStrict $ mail) <> "\" is already used." }

afterGet _id action = do
    let key = toSqlKey $ fromIntegral _id :: Key User
    gotUser <- runDB $ get key
    case gotUser of
      Just _ -> action key
      Nothing -> throwError $ err404 { errBody = "user with id = " <> (pack . show) _id <> "doesn't exist." }

main :: IO ()
main = Warp.run 8080 $ serve userApi server
