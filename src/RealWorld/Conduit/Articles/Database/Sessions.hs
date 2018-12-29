{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RealWorld.Conduit.Articles.Database.Sessions
  ( all
  , feed
  , find
  , insert
  , update
  , runSession
  , Error(..)
  , ValidationError(..)
  ) where

import Control.Monad (join)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Validation (Validation(Failure, Success), toEither)
import Data.Word (Word64)
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, Session)
import qualified Hasql.Session as Session
import Prelude hiding (all)
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Article as Article
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

newtype ValidationError = ValidationError { getValidationError :: Map Text [Text] }
  deriving Semigroup

require :: Text -> Text -> Validation ValidationError Text
require attr value =
  if Text.null value
    then Failure $ ValidationError $ Map.singleton attr ["Required"]
    else Success value

newtype Title = Title { getTitle :: Text }

makeTitle
  :: Text
  -> Compose Session.Session (Validation ValidationError) Title
makeTitle title = Compose $ do
  let slug = generateSlug title
  existing <- find Nothing slug
  pure $ require "title" title *> case existing of
    Nothing -> Success $ Title title
    Just _  -> Failure $ ValidationError $ Map.singleton
      "title"
      ["Would produce duplicate slug: " <> slug]

newtype Description = Description { getDescription :: Text }

makeDescription :: Text -> Validation ValidationError Description
makeDescription = (Description <$>) . require "description"

newtype Body = Body { getBody :: Text }

makeBody :: Text -> Validation ValidationError Body
makeBody = (Body <$>) . require "body"

newtype TagsList = TagsList { getTagsList :: Set Text }

type family Attribute f a where
  Attribute Identity a = a
  Attribute Maybe a = Maybe a

data Params f = Params
  { pTitle :: Attribute f Title
  , pDescription :: Attribute f Description
  , pBody :: Attribute f Body
  , pTagsList :: Attribute f TagsList
  }

generateSlug :: Text -> Text
generateSlug = Text.intercalate "-" . Text.words . Text.toLower . Text.filter
  ((||) <$> Char.isAlphaNum <*> Char.isSpace)

runSession
  :: ExceptT Error Session a -> Connection -> ExceptT Error IO a
runSession session =
  ExceptT
    . (join <$>)
    . runExceptT
    . withExceptT QueryFailure
    . ExceptT
    . Session.run (runExceptT session)

makeInsertParams
  :: Text
  -> Text
  -> Text
  -> Set Text
  -> Compose
       Session
       (Validation ValidationError)
       (Params Identity)
makeInsertParams title description body tags =
  Params
    <$> makeTitle title
    <*> Compose (pure (makeDescription description))
    <*> Compose (pure (makeBody body))
    <*> pure (TagsList tags)

insertParams
  :: Text
  -> Text
  -> Text
  -> Set Text
  -> ExceptT ValidationError Session (Params Identity)
insertParams title description body tags = ExceptT $ toEither <$> getCompose
  (makeInsertParams title description body tags)

data Error
  = NotFound
  | QueryFailure QueryError
  | ValidationFailure ValidationError

insert
  :: Int
  -> UTCTime
  -> Text
  -> Text
  -> Text
  -> Set Text
  -> ExceptT Error Session Article
insert authorId time title description body tags = do
  params <- withExceptT ValidationFailure $ insertParams title description body tags
  let slug = generateSlug title
  articleId <- lift $ Session.statement
    ( slug
    , getTitle . pTitle $ params
    , getDescription . pDescription $ params
    , getBody . pBody $ params
    , authorId
    , time
    )
    Statements.insertArticle
  lift $ do
    updateTags articleId (getTagsList . pTagsList $ params)
    decorateArticle (Just authorId) slug

makeUpdateTitle
  :: Text
  -> Text
  -> Compose Session (Validation ValidationError) Title
makeUpdateTitle current title
  | title == current = pure (Title title)
  | otherwise = makeTitle title

makeUpdateParams
  :: Article
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Set Text)
  -> Compose
       Session
       (Validation ValidationError)
       (Params Maybe)
makeUpdateParams current title description body tags =
  Params
    <$> traverse (makeUpdateTitle (Article.title current)) title
    <*> traverse (Compose . pure . makeDescription) description
    <*> traverse (Compose . pure . makeBody) body
    <*> traverse (pure . TagsList) tags

updateParams
  :: Article
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Set Text)
  -> ExceptT ValidationError Session (Params Maybe)
updateParams current title description body tags =
  ExceptT $ toEither <$> getCompose
    (makeUpdateParams current title description body tags)

update
  :: Int
  -> Text
  -> UTCTime
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Set Text)
  -> ExceptT Error Session Article
update currentUser currentSlug time title description body tags = do
  current <- maybeToExceptT NotFound
    $ MaybeT $ find (Just currentUser) currentSlug
  params <- withExceptT ValidationFailure
    $ updateParams current title description body tags
  lift $ do
    (updatedId, updatedSlug) <- Session.statement
      ( currentSlug
      , generateSlug <$> title
      , (getTitle <$>) . pTitle $ params
      , (getDescription <$>) . pDescription $ params
      , (getBody <$>) . pBody $ params
      , time
      )
      Statements.updateArticle
    traverse_ (updateTags updatedId) tags
    decorateArticle (Just currentUser) updatedSlug
