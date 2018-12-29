module RealWorld.Conduit.Articles.Database
  ( all
  , feed
  , find
  , insert
  , update
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.Session (QueryError)
import Prelude hiding (all)
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Database.Sessions as Sessions

all
  :: Maybe Int
  -> Word64
  -> Word64
  -> Set Text
  -> Set Text
  -> Connection
  -> IO (Either QueryError [Article])
all currentUser lim off tags usernames =
  Session.run $ Sessions.all currentUser lim off tags usernames

feed
  :: Maybe Int
  -> Word64
  -> Word64
  -> Set Text
  -> Set Text
  -> Connection
  -> IO (Either QueryError [Article])
feed currentUser lim off tags usernames =
  Session.run $ Sessions.feed currentUser lim off tags usernames

find
  :: Maybe Int -> Text -> Connection -> IO (Either QueryError (Maybe Article))
find currentUser slug =
  Session.run $ Sessions.find currentUser slug

insert
  :: Int
  -> Text
  -> Text
  -> Text
  -> Text
  -> Set Text
  -> Connection
  -> IO (Either QueryError Article)
insert currentUser slug title description body tags conn = do
  time <- getCurrentTime
  Session.run
    (Sessions.insert slug title description body currentUser time tags)
    conn

update
  :: Int
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Set Text)
  -> Connection
  -> IO (Either QueryError (Maybe Article))
update currentUser oldSlug slug title description body tags conn = do
  time <- getCurrentTime
  Session.run
    (Sessions.update currentUser oldSlug slug title description body time tags)
    conn
