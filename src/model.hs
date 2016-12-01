{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Data.Text (Text)
import           Data.Time.Clock (UTCTime(..))
import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    mail Text
    name Text
    phoneNumber Text
    UniqueMail mail
    deriving Show
  Car
    name Text
  Reservation
    userId UserId
    carId CarId
    begin UTCTime
    end UTCTime
    updated UTCTime
    created UTCTime
    deriving Show
  Occupation
    userId UserId
    carId CarId
    begin UTCTime
    end UTCTime
    meterBegin UTCTime
    meterEnd UTCTime
    updated UTCTime
    created UTCTime
    deriving Show
|]
