{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RealWorld.Api
  ( runApi
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Servant                        ( (:>)
                                                , Get
                                                , Post
                                                , JSON
                                                , ReqBody
                                                , Server
                                                , Handler
                                                , serve
                                                , Application
                                                )
import           RealWorld.Api.Types            ( UserBody(..)
                                                , RegistrationData(..)
                                                , User(..)
                                                )
import           Control.Monad.Except           ( liftIO )
import qualified RealWorld.Database            as DB
import           Database.Persist.Postgresql    ( ConnectionPool
                                                , withPostgresqlPool
                                                , runSqlPersistMPool
                                                , runMigrationUnsafe
                                                )
import           Control.Monad.Logger           ( runStderrLoggingT )
import qualified Network.Wai.Handler.Warp      as Warp

type UserAPI = "users" :>
  ReqBody '[JSON] (UserBody RegistrationData) :> Post '[JSON] (UserBody User)

type API = "api" :> UserAPI

server :: ConnectionPool -> Server API
server pool = register pool

api :: Proxy API
api = Proxy

register
  :: ConnectionPool -> UserBody RegistrationData -> Handler (UserBody User)
register pool UserBody { user = reg@RegistrationData { email, username, password } }
  = do
    _ <- liftIO $ DB.createUser pool reg
    pure $ UserBody
      (User
        { username = username
        , email    = email
        , token    = "dymmy"
        , image    = Nothing
        , bio      = Nothing
        }
      )

app :: ConnectionPool -> Application
app pool = serve api (server pool)

connStr =
  "host=localhost dbname=realworld user=postgres password=postgres port=5432"

mkApp :: IO Application
mkApp = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
  liftIO $ flip runSqlPersistMPool pool $ do
    runMigrationUnsafe DB.migrateAll
    pure $ app pool

runApi :: IO ()
runApi = Warp.run 5000 =<< mkApp
