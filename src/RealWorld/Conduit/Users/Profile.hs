{-# OPTIONS_GHC -Wall #-}

module RealWorld.Conduit.Users.Profile where

import Data.Text (Text)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  } deriving (Show)
