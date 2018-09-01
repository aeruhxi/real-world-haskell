{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RealWorld.Api.Types
  ( User(..)
  , Profile(..)
  , Article(..)
  , Comment(..)
  , Tag(..)
  , Errors(..)
  , UserBody(..)
  , ArticleBody(..)
  , CommentBody(..)
  , LoginData(..)
  , RegistrationData(..)
  , AuthenticatedUser(..)
  )
where

import           Data.Aeson      (FromJSON (..), ToJSON (..))
import           Data.Int        (Int64)
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           GHC.Generics


-- Models for JSON
data User = User
  { email    :: Text
  , token    :: Text
  , username :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Show, Generic, ToJSON)

data AuthenticatedUser = AuthenticatedUser
  { userId :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)


data Profile = Profile
  { username  :: Text
  , bio       :: Text
  , image     :: Text
  , following :: Bool
  } deriving (Show, Generic, ToJSON)

data Article = Article
  { slug           :: Text
  , title          :: Text
  , description    :: Text
  , body           :: Text
  , tags           :: [Tag]
  , createdAt      :: UTCTime
  , updatedAt      :: UTCTime
  , favorited      :: Bool
  , favoritesCount :: Int64
  , author         :: Profile
  } deriving (Show, Generic, ToJSON)

data Comment = Comment
  { id        :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , body      :: Text
  , author    :: Profile
  } deriving (Show, Generic, ToJSON)

newtype Tag = Tag { unTag :: String } deriving (Show, Generic, ToJSON)

newtype Errors = Errors
  { body :: [Text]
  } deriving (Show, Generic, ToJSON)


-- Request Body Types
data LoginData = LoginData
  { username :: Text
  , password :: Text
  } deriving (Show, Generic, FromJSON)

data RegistrationData = RegistrationData
  { username :: Text
  , password :: Text
  , email    :: Text
  } deriving (Show, Generic, FromJSON)


-- Generic Body Types
newtype UserBody a = UserBody { user :: a }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ArticleBody a = AricleBody { article :: a }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype CommentBody a = CommentBody { comment :: a }
  deriving (Show, Generic, ToJSON, FromJSON)
