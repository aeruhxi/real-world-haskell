{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module RealWorld.Database
  ( User(..)
  , createUser
  , migrateAll
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Database.Persist.Postgresql    ( runSqlPersistMPool
                                                , insert
                                                , delete
                                                , Entity(..)
                                                , fromSqlKey
                                                , ConnectionPool
                                                )
import           Database.Persist.TH            ( mkMigrate
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                , sqlSettings
                                                , mkPersist
                                                )
import           Data.Text                      ( Text )
import           RealWorld.Api.Types            ( RegistrationData(..) )
import           Data.Int                       ( Int64 )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  email Text sqlType=varchar(320)
  UniqueEmail email

  username Text sqlType=varchar(50)
  UniqueUsername username

  password Text

  deriving Show
|]

-- Model functions
createUser :: ConnectionPool -> RegistrationData -> IO Int64
createUser pool RegistrationData { username, email, password } =
  flip runSqlPersistMPool pool $ do
    userId <- insert $ User email username password
    pure (fromSqlKey userId)
