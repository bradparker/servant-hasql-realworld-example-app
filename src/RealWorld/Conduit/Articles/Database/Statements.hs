module RealWorld.Conduit.Articles.Database.Statements
  ( selectFilteredArticles
  , selectFeedArticles
  , selectArticle
  , selectKnownArticle
  , insertArticle
  , deleteTags
  , insertTag
  , updateArticle
  ) where

import Control.Lens (_1, _2, _3, _4, _5, _6, view)
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.Functor.Contravariant (contramap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Hasql.Statement (Statement(Statement))
import Prelude hiding (all)
import RealWorld.Conduit.Articles.Article (Article(Article))
import RealWorld.Conduit.Database.Schema (Schema)
import RealWorld.Conduit.Users.Database (profileRow)
import Squeal.PostgreSQL.Extended
  ( (:::)
  , ColumnValue(Default, Same, Set)
  , ConflictClause(OnConflictDoRaise)
  , Manipulation
  , NP((:*))
  , NullityType(NotNull, Null)
  , PGType(..)
  , Query
  , ReturningClause(Returning)
  , SortExpression(Asc)
  , (!)
  , (&)
  , (.==)
  , arrayAgg
  , arrayCardinality
  , arrayOverlap
  , as
  , boolOr
  , coalesce
  , count
  , deleteFrom_
  , false
  , from
  , fromNull
  , groupBy
  , ifThenElse
  , in_
  , innerJoin
  , insertRow
  , insertRow_
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
  , update
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
    (  (#articles ! #slug `as` #article_slug)
    :* (#articles ! #title `as` #article_title)
    :* (#articles ! #description `as` #article_description)
    :* (#articles ! #body `as` #article_body)
    :* (arrayAgg (#tags ! #name) `as` #article_tags_list)
    :* (#articles ! #created_at `as` #article_created_at)
    :* (#articles ! #updated_at `as` #article_updated_at)
    :* (boolOr
         (ifThenElse
           (isNull (param @1))
           false
           (fromNull false (#favorites ! #user_id .== param @1)))
       `as` #article_favorited
       )
    :* (count (#favorites ! #user_id) `as` #article_favorites_count)
    :* (#authors ! #username `as` #author_username)
    :* (#authors ! #bio `as` #author_bio)
    :* (#authors ! #image `as` #author_image)
    :* (boolOr
         (ifThenElse
           (isNull (param @1))
           false
           (fromNull false (#follows ! #follower_id .== param @1)))
       `as` #author_following
       )
    )

    $ from
        ( table (#articles `as` #articles)
        & leftOuterJoin
            (table (#favorites `as` #favorites))
            (#articles ! #id .== #favorites ! #article_id)
        & leftOuterJoin
            (table (#tags `as` #tags))
            (#articles ! #id .== #tags ! #article_id)
        & innerJoin
            (table (#users `as` #authors))
            (#articles ! #author_id .== #authors ! #id)
        & leftOuterJoin
            (table (#follows `as` #follows))
            (#authors ! #id .== #follows ! #followee_id)
        )

    & groupBy
        (  (#articles ! #slug)
        :* (#articles ! #title)
        :* (#articles ! #description)
        :* (#articles ! #body)
        :* (#articles ! #created_at)
        :* (#articles ! #updated_at)
        :* (#authors ! #username)
        :* (#authors ! #bio)
        :* (#authors ! #image)
        )

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
        (ifThenElse
          (arrayCardinality (param @2) .== 0)
          true
          (arrayOverlap (param @2) (#filtered_articles ! #article_tags_list))
        .== true)
    & where_
        (ifThenElse
          (arrayCardinality (param @3) .== 0)
          true
          (#filtered_articles ! #author_username `in_` unnest (param @3)))
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
selectFilteredArticles lim off = Statement sql encoder decoder False
  where
    sql = renderSQL (selectFilteredArticlesQuery lim off)
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param (foldableEncoder Encoders.text)) <>
      contramap (view _3) (Encoders.param (foldableEncoder Encoders.text))
    decoder = Decoders.rowList articleRow

selectFeedArticlesQuery
  :: Word64 -> Word64 -> Query Schema FilterParams ArticleRow
selectFeedArticlesQuery lim off =
  selectStar
    $ from
        (subquery (selectFilteredArticlesQuery lim off `as` #filtered_articles))
    & where_ (#filtered_articles ! #author_following .== true)

selectFeedArticles
  :: Word64 -> Word64 -> Statement (Maybe Int, Set Text, Set Text) [Article]
selectFeedArticles lim off = Statement sql encoder decoder False
  where
    sql = renderSQL (selectFeedArticlesQuery lim off)
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param (foldableEncoder Encoders.text)) <>
      contramap (view _3) (Encoders.param (foldableEncoder Encoders.text))
    decoder = Decoders.rowList articleRow

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

selectKnownArticle :: Statement (Maybe Int, Text) Article
selectKnownArticle = Statement sql encoder decoder True
  where
    sql = renderSQL selectArticleQuery
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param Encoders.text)
    decoder = Decoders.singleRow articleRow

insertArticleManipulation
  :: Manipulation
       Schema
       '[ 'NotNull 'PGtext
        , 'NotNull 'PGtext
        , 'NotNull 'PGtext
        , 'NotNull 'PGtext
        , 'NotNull 'PGint8
        , 'NotNull 'PGtimestamptz
        ]
       '["id" ::: 'NotNull 'PGint8]
insertArticleManipulation = insertRow
  #articles
  (  (Default `as` #id)
  :* (Set (param @1) `as` #slug)
  :* (Set (param @2) `as` #title)
  :* (Set (param @3) `as` #description)
  :* (Set (param @4) `as` #body)
  :* (Set (param @5) `as` #author_id)
  :* (Set (param @6) `as` #created_at)
  :* (Set (param @6) `as` #updated_at)
  )
  OnConflictDoRaise
  (Returning (#id `as` #id))

insertArticle :: Statement (Text, Text, Text, Text, Int, UTCTime) Int
insertArticle = Statement sql encoder decoder True
  where
    sql = renderSQL insertArticleManipulation
    encoder =
      contramap (view _1) (Encoders.param Encoders.text) <>
      contramap (view _2) (Encoders.param Encoders.text) <>
      contramap (view _3) (Encoders.param Encoders.text) <>
      contramap (view _4) (Encoders.param Encoders.text) <>
      contramap (fromIntegral . view _5) (Encoders.param Encoders.int8) <>
      contramap (view _6) (Encoders.param Encoders.timestamptz)
    decoder = Decoders.singleRow (Decoders.column (fromIntegral <$> Decoders.int8))

deleteTagsManipulation :: Manipulation Schema '[ 'NotNull 'PGint8] '[]
deleteTagsManipulation = deleteFrom_ #tags (#article_id .== param @1)

deleteTags :: Statement Int ()
deleteTags = Statement sql encoder decoder True
  where
    sql = renderSQL deleteTagsManipulation
    encoder = contramap fromIntegral (Encoders.param Encoders.int8)
    decoder = Decoders.unit

insertTagManipulation
  :: Manipulation Schema '[ 'NotNull 'PGint8, 'NotNull 'PGtext] '[]
insertTagManipulation =
  insertRow_ #tags
  (  (Set (param @1) `as` #article_id)
  :* (Set (param @2) `as` #name)
  )

insertTag :: Statement (Int, Text) ()
insertTag = Statement sql encoder decoder True
  where
    sql = renderSQL insertTagManipulation
    encoder =
      contramap (fromIntegral . view _1) (Encoders.param Encoders.int8) <>
      contramap (view _2) (Encoders.param Encoders.text)
    decoder = Decoders.unit

updateArticleManipulation
  :: Manipulation
       Schema
       '[ 'NotNull 'PGtext
        , 'Null 'PGtext
        , 'Null 'PGtext
        , 'Null 'PGtext
        , 'Null 'PGtext
        , 'NotNull 'PGtimestamptz
        ]
       '[ "id" ::: 'NotNull 'PGint8
        , "slug" ::: 'NotNull 'PGtext
        ]
updateArticleManipulation = update
  #articles
  (  (Same `as` #id)
  :* (Set (coalesce [param @2] #slug) `as` #slug)
  :* (Set (coalesce [param @3] #title) `as` #title)
  :* (Set (coalesce [param @4] #description) `as` #description)
  :* (Set (coalesce [param @5] #body) `as` #body)
  :* (Same `as` #author_id)
  :* (Same `as` #created_at)
  :* (Set (param @6) `as` #updated_at)
  )
  (#slug .== param @1)
  (Returning (#id :* #slug))

updateArticle
  :: Statement
       (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, UTCTime)
       (Int, Text)
updateArticle = Statement sql encoder decoder True
 where
  sql = renderSQL updateArticleManipulation
  encoder =
    contramap (view _1) (Encoders.param Encoders.text) <>
    contramap (view _2) (Encoders.nullableParam Encoders.text) <>
    contramap (view _3) (Encoders.nullableParam Encoders.text) <>
    contramap (view _4) (Encoders.nullableParam Encoders.text) <>
    contramap (view _5) (Encoders.nullableParam Encoders.text) <>
    contramap (view _6) (Encoders.param Encoders.timestamptz)
  decoder =
    Decoders.singleRow $
      (,)
        <$> (fromIntegral <$> Decoders.column Decoders.int8)
        <*> Decoders.column Decoders.text
