{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified RealWorld.Conduit.Articles.Database as Articles
import Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Char8 as BS
import qualified Hasql.Connection as Connection
import Hasql.Connection (Connection)
import Prelude hiding (id)
import System.Environment (getEnv)

newtype ConnectionError = ConnectionError
  { getConnectionError :: Connection.ConnectionError
  } deriving (Show)

instance Exception ConnectionError

connect :: Connection.Settings -> IO Connection
connect = (either (throwIO . ConnectionError) pure =<<) . Connection.acquire

main :: IO ()
main = do
  databaseUrl <- BS.pack <$> getEnv "DATABASE_URL"
  conn <- connect databaseUrl
  let query = Articles.defaultArticleQuery
  print =<< Articles.all Nothing 20 0 query conn
  print =<< Articles.feed Nothing 20 0 query conn
  print =<< Articles.find Nothing "a-thing" conn
