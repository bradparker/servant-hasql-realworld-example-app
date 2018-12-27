module RealWorld.Conduit.Articles.Database
  ( all
  , feed
  , find
  ) where

import Control.Lens (_1, _2, _3, view)
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.Functor.Contravariant (contramap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import qualified Hasql.Session as Session
import Hasql.Session (QueryError)
import Hasql.Statement (Statement(Statement))
import Prelude hiding (all)
import RealWorld.Conduit.Articles.Article (Article(Article))
import RealWorld.Conduit.Users.Database (profileRow)
import RealWorld.Conduit.Database.Schema (Schema)
import Squeal.PostgreSQL.Extended
  ( (:::)
  , NP((:*))
  , NullityType(NotNull, Null)
  , PGType(..)
  , Query
  , SortExpression(Asc)
  , (!)
  , (&)
  , (.==)
  , arrayAgg
  , arrayCardinality
  , arrayOverlap
  , as
  , boolOr
  , count
  , false
  , from
  , fromNull
  , groupBy
  , ifThenElse
  , in_
  , innerJoin
  , isNull
  , leftOuterJoin
  , limit
  , offset
  , orderBy
  , param
  , renderSQL
  , select
  , selectStar
  , subquery
  , table
  , true
  , unnest
  , where_
  )

type ArticleRow =
  '[ "article_slug" ::: 'NotNull 'PGtext
   , "article_title" ::: 'NotNull 'PGtext
   , "article_description" ::: 'NotNull 'PGtext
   , "article_body" ::: 'NotNull 'PGtext
   , "article_tags_list" ::: 'NotNull ('PGvararray ('NotNull 'PGtext))
   , "article_created_at" ::: 'NotNull 'PGtimestamptz
   , "article_updated_at" ::: 'NotNull 'PGtimestamptz
   , "article_favorited" ::: 'NotNull 'PGbool
   , "article_favorites_count" ::: 'NotNull 'PGint8

   , "author_username" ::: 'NotNull 'PGtext
   , "author_bio" ::: 'NotNull 'PGtext
   , "author_image" ::: 'Null 'PGtext
   , "author_following" ::: 'NotNull 'PGbool
   ]

selectArticlesQuery :: Query Schema ('Null 'PGint8 : params) ArticleRow
selectArticlesQuery =
  select
    ( #articles ! #slug
        `as` #article_slug :*
      #articles ! #title
        `as` #article_title :*
      #articles ! #description
        `as` #article_description :*
      #articles ! #body
        `as` #article_body :*
      arrayAgg (#article_tags ! #tag__name)
        `as` #article_tags_list :*
      #articles ! #created_at
        `as` #article_created_at :*
      #articles ! #updated_at
        `as` #article_updated_at :*
      boolOr
        (ifThenElse
          (isNull (param @1))
          false
          (fromNull false (#favorites ! #user__id .== param @1)))
        `as` #article_favorited :*
      count (#favorites ! #user__id)
        `as` #article_favorites_count :*

      #authors ! #username
        `as` #author_username :*
      #authors ! #bio
        `as` #author_bio :*
      #authors ! #image
        `as` #author_image :*
      boolOr
        (ifThenElse
          (isNull (param @1))
          false
          (fromNull false (#follows ! #follower__id .== param @1)))
        `as` #author_following
    )

  $ from (table (#articles `as` #articles)
      & leftOuterJoin
          (table (#favorites `as` #favorites))
          (#articles ! #id .== #favorites ! #article__id)
      & leftOuterJoin
          (table (#article_tags `as` #article_tags))
          (#articles ! #id .== #article_tags ! #article__id)
      & innerJoin
          (table (#users `as` #authors))
          (#articles ! #author__id .== #authors ! #id)
      & leftOuterJoin
          (table (#follows `as` #follows))
          (#authors ! #id .== #follows ! #followee__id))

  & groupBy
      ( #articles ! #slug :*
        #articles ! #title :*
        #articles ! #description :*
        #articles ! #body :*
        #articles ! #created_at :*
        #articles ! #updated_at :*

        #authors ! #username :*
        #authors ! #bio :*
        #authors ! #image )

type FilterParams =
  '[ 'Null 'PGint8
   , 'NotNull ('PGvararray ('NotNull 'PGtext))
   , 'NotNull ('PGvararray ('NotNull 'PGtext))
   ]

selectFilteredArticlesQuery
  :: Word64 -> Word64 -> Query Schema FilterParams ArticleRow
selectFilteredArticlesQuery lim off =
  selectStar
    $ from (subquery (selectArticlesQuery `as` #filtered_articles))
    & where_
        (   ifThenElse
            (arrayCardinality (param @2) .== 0)
            true
            (arrayOverlap (param @2) (#filtered_articles ! #article_tags_list))
        .== true
        )
    & where_
        (ifThenElse
          (arrayCardinality (param @3) .== 0)
          true
          (#filtered_articles ! #author_username `in_` unnest (param @3))
        )
    & orderBy [#filtered_articles ! #article_created_at & Asc]
    & limit lim
    & offset off

tagsValue :: Decoders.Value (Set Text)
tagsValue = Set.fromList <$> Decoders.array
  (Decoders.dimension replicateM (Decoders.element Decoders.text))

articleRow :: Row Article
articleRow =
  Article
    <$> Decoders.column Decoders.text
    <*> Decoders.column Decoders.text
    <*> Decoders.column Decoders.text
    <*> Decoders.column Decoders.text
    <*> Decoders.column tagsValue
    <*> Decoders.column Decoders.timestamptz
    <*> Decoders.column Decoders.timestamptz
    <*> Decoders.column Decoders.bool
    <*> (fromIntegral <$> Decoders.column Decoders.int8)
    <*> profileRow

foldableEncoder :: Foldable t => Encoders.Value a -> Encoders.Value (t a)
foldableEncoder value =
  Encoders.array (Encoders.dimension foldl' (Encoders.element value))

currentUserIdEncoder :: Params (Maybe Int)
currentUserIdEncoder =
  contramap (fmap fromIntegral) (Encoders.nullableParam Encoders.int8)

selectFilteredArticles
  :: Word64 -> Word64 -> Statement (Maybe Int, Set Text, Set Text) [Article]
selectFilteredArticles lim off = Statement sql encoder decoder True
  where
    sql = renderSQL (selectFilteredArticlesQuery lim off)
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param (foldableEncoder Encoders.text)) <>
      contramap (view _3) (Encoders.param (foldableEncoder Encoders.text))
    decoder = Decoders.rowList articleRow

all
  :: Maybe Int
  -> Word64
  -> Word64
  -> Set Text
  -> Set Text
  -> Connection
  -> IO (Either QueryError [Article])
all currentUser lim off tags usernames = Session.run
  (Session.statement (currentUser, tags, usernames) (selectFilteredArticles lim off))

selectFeedArticlesQuery
  :: Word64 -> Word64 -> Query Schema FilterParams ArticleRow
selectFeedArticlesQuery lim off =
  selectStar
    $ from
        (subquery (selectFilteredArticlesQuery lim off `as` #filtered_articles))
    & where_ (#filtered_articles ! #author_following .== true)

selectFeedArticles
  :: Word64 -> Word64 -> Statement (Maybe Int, Set Text, Set Text) [Article]
selectFeedArticles lim off = Statement sql encoder decoder True
  where
    sql = renderSQL (selectFeedArticlesQuery lim off)
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param (foldableEncoder Encoders.text)) <>
      contramap (view _3) (Encoders.param (foldableEncoder Encoders.text))
    decoder = Decoders.rowList articleRow

feed
  :: Maybe Int
  -> Word64
  -> Word64
  -> Set Text
  -> Set Text
  -> Connection
  -> IO (Either QueryError [Article])
feed currentUser lim off tags usernames = Session.run
  (Session.statement (currentUser, tags, usernames) (selectFeedArticles lim off))

selectArticleQuery
  :: Query Schema '[ 'Null 'PGint8, 'NotNull 'PGtext] ArticleRow
selectArticleQuery =
  selectStar
    $ from (subquery (selectArticlesQuery `as` #filtered_articles))
    & where_ (#filtered_articles ! #article_slug .== param @2)

selectArticle :: Statement (Maybe Int, Text) (Maybe Article)
selectArticle = Statement sql encoder decoder True
  where
    sql = renderSQL selectArticleQuery
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param Encoders.text)
    decoder = Decoders.rowMaybe articleRow

find
  :: Maybe Int -> Text -> Connection -> IO (Either QueryError (Maybe Article))
find currentUser slug =
  Session.run (Session.statement (currentUser, slug) selectArticle)
