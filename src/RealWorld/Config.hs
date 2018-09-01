module RealWorld.Config
  ( Config(..)
  , AppM
  )
where

import           Control.Monad.Reader        (ReaderT)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (ConnectionPool)
import           Servant                     (Handler)

data Config = Config
  { configPool      :: ConnectionPool
  , configJwtSecret :: Text
  }

type AppM = ReaderT Config Handler
