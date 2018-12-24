{-# OPTIONS_GHC -Wall #-}

module RealWorld.Conduit.Articles.Article where

import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import RealWorld.Conduit.Users.Profile (Profile)

data Article = Article
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: Set Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  } deriving (Show)
