module RealWorld.Conduit.Articles.Database
  ( all
  , feed
  , find
  , insert
  , update
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
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
  -> Set Text
  -> Connection
  -> ExceptT Sessions.Error IO Article
insert currentUser title description body tags conn = do
  time <- lift getCurrentTime
  Sessions.runSession
    (Sessions.insert currentUser time title description body tags)
    conn

update
  :: Int
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Set Text)
  -> Connection
  -> ExceptT Sessions.Error IO Article
update currentUser currentSlug title description body tags conn = do
  time <- lift getCurrentTime
  Sessions.runSession
    (Sessions.update currentUser currentSlug time title description body tags)
    conn
