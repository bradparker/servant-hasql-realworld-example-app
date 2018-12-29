module RealWorld.Conduit.Articles.Database.Sessions
  ( all
  , feed
  , find
  , insert
  , update
  ) where

import Control.Lens (_1, _2, view)
import Data.Time (UTCTime)
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Prelude hiding (all)
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Database.Statements as Statements

all
  :: Maybe Int -> Word64 -> Word64 -> Set Text -> Set Text -> Session [Article]
all currentUser lim off tags usernames = Session.statement
  (currentUser, tags, usernames)
  (Statements.selectFilteredArticles lim off)

feed
  :: Maybe Int -> Word64 -> Word64 -> Set Text -> Set Text -> Session [Article]
feed currentUser lim off tags usernames = Session.statement
  (currentUser, tags, usernames)
  (Statements.selectFeedArticles lim off)

find :: Maybe Int -> Text -> Session (Maybe Article)
find currentUser slug =
  Session.statement (currentUser, slug) Statements.selectArticle

updateTags :: Int -> Set Text -> Session ()
updateTags articleId tags = do
  Session.statement articleId Statements.deleteTags
  traverse_ (\tag -> Session.statement (articleId, tag) Statements.insertTag) tags

decorateArticle :: Maybe Int -> Text -> Session Article
decorateArticle currentUser slug =
  Session.statement (currentUser, slug) Statements.selectKnownArticle

insert
  :: Text
  -> Text
  -> Text
  -> Text
  -> Int
  -> UTCTime
  -> Set Text
  -> Session Article
insert slug title description body authorId time tags = do
  articleId <- Session.statement
    (slug, title, description, body, authorId, time, time)
    Statements.insertArticle
  updateTags articleId tags
  decorateArticle (Just authorId) slug

update
  :: Int
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> UTCTime
  -> Maybe (Set Text)
  -> Session (Maybe Article)
update currentUser oldSlug slug title description body time tags = do
  updateResult <- Session.statement
    (oldSlug, slug, title, description, body, time)
    Statements.updateArticle
  traverse_ (uncurry updateTags) $ (,) <$> (view _1 <$> updateResult) <*> tags
  traverse (decorateArticle (Just currentUser) . view _2) updateResult
