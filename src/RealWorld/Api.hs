{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module RealWorld.Api
  ( runApi
  )
where

import           Control.Monad.Except             (liftIO)
import           Control.Monad.Logger             (runStderrLoggingT)
import           Data.Proxy                       (Proxy (..))
import           Database.Persist.Postgresql      (ConnectionPool, Entity (..),
                                                   getBy, insert,
                                                   runMigrationUnsafe,
                                                   runSqlPersistMPool,
                                                   withPostgresqlPool)
import           Network.Wai                      (Request, requestHeaders)
import           RealWorld.Api.Types              (AuthenticatedUser (..),
                                                   LoginData (..),
                                                   RegistrationData (..),
                                                   User (..), UserBody (..))
import qualified RealWorld.Database               as DB
import           Servant                          ((:<|>) ((:<|>)), (:>),
                                                   Application, Context,
                                                   Context ((:.), EmptyContext),
                                                   Get, Handler, JSON, Post,
                                                   ReqBody, Server, ServerT,
                                                   err401, err404, err500,
                                                   hoistServer,
                                                   serveWithContext, throwError)
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)

import           Control.Monad.Reader             (asks, runReaderT)
import           Control.Monad.Reader             (asks)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Crypto.BCrypt                    (hashPasswordUsingPolicy,
                                                   hashUsesPolicy,
                                                   slowerBcryptHashingPolicy,
                                                   validatePassword)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Types                 as Aeson
import qualified Data.ByteString.Char8            as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T
import qualified Network.Wai.Handler.Warp         as Warp
import           RealWorld.Config                 (AppM, Config (..))
import           Web.JWT                          (JWTClaimsSet (..))
import qualified Web.JWT                          as Jwt


-- API Specs ------------------------------------------------------------------

type RegisterApi = ReqBody '[JSON] (UserBody RegistrationData)
                 :> Post '[JSON] (UserBody User)

type LoginApi = "login"
              :> ReqBody '[JSON] (UserBody LoginData)
              :> Post '[JSON] (UserBody User)

type UserAPI = "users" :> (RegisterApi :<|> LoginApi)


type API = "api" :> UserAPI


-- Handlers -------------------------------------------------------------------

register :: UserBody RegistrationData -> AppM (UserBody User)
register UserBody { user = RegistrationData { email, username, password } } = do
  pwh <- liftIO $ hashPassword password
  case pwh of
    Just pwh -> do
      DB.runDb $ insert (DB.User email username pwh)
      token <- createToken
      pure $ UserBody User { username = username
                           , email = email
                           , token = token
                           , image = Nothing
                           , bio = Nothing
                           }
    Nothing -> throwError err500
  where
    hashPassword pw =
      hashPasswordUsingPolicy slowerBcryptHashingPolicy (T.encodeUtf8 pw)

login :: UserBody LoginData -> AppM (UserBody User)
login UserBody { user = LoginData { username, password } } = do
  user <- DB.runDb $ getBy $ DB.UniqueUsername username
  case user of
    Nothing -> throwError err404
    Just (Entity _ user@DB.User { userPassword, userEmail, userUsername }) ->
      if validatePassword userPassword (T.encodeUtf8 password)
        then createToken >>= \token ->
               pure $ UserBody $ User { email = userEmail
                                      , token = token
                                      , username = userUsername
                                      , bio = Nothing
                                      , image = Nothing
                                      }
        else throwError err401


serverHandler :: ServerT API AppM
serverHandler = register :<|> login

-- Configuring app and db -----------------------------------------------------

authHandler :: Config -> AuthHandler Request AuthenticatedUser
authHandler Config { configJwtSecret = jwtSecret } = mkAuthHandler handler
 where
   handler :: Request -> Handler AuthenticatedUser
   handler req = case getUserId req of
     Nothing -> throwError err401
     Just id -> pure $ AuthenticatedUser id

   getUserId :: Request -> Maybe Text
   getUserId req = do
     authHeader <- lookup "Authorization" (requestHeaders req)
     token <- BS.stripPrefix "Token " authHeader
     sig <- Jwt.decodeAndVerifySignature (Jwt.secret jwtSecret) (T.decodeUtf8 token)
     let claimsMap = unregisteredClaims (Jwt.claims sig)
     Map.lookup "userId" claimsMap >>= Aeson.parseMaybe Aeson.parseJSON


server :: Config -> Server API
server config = hoistServer api (`runReaderT` config) serverHandler

api :: Proxy API
api = Proxy

authContext :: Config -> Context (AuthHandler Request AuthenticatedUser ': '[])
authContext config = authHandler config :. EmptyContext

app :: Config -> Application
app config = serveWithContext api (authContext config) (server config)

connStr =
  "host=localhost dbname=realworld user=postgres password=postgres port=5432"

mkApp :: IO Application
mkApp = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
  liftIO $ flip runSqlPersistMPool pool $ do
    runMigrationUnsafe DB.migrateAll
    pure $ app (Config pool "superdevsecret")

port = 5000

runApi :: IO ()
runApi = do
  print $ "Server is running at http://localhost:" <> (show port)
  Warp.run port =<< mkApp


-- Helpers
createToken :: AppM Text
createToken =
  asks configJwtSecret >>= \s ->
    pure $ Jwt.encodeSigned Jwt.HS256 (Jwt.secret s) Jwt.def

