{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module RealWorld.Database

where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (asks)
import           Data.ByteString             (ByteString)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (ConnectionPool, Entity (..),
                                              delete, fromSqlKey, insert,
                                              runSqlPersistMPool)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           RealWorld.Api.Types         (RegistrationData (..))
import           RealWorld.Config            (AppM, Config (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  email Text sqlType=varchar(320)
  UniqueEmail email

  username Text sqlType=varchar(50)
  UniqueUsername username

  password ByteString

  deriving Show
|]

runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPersistMPool query pool
