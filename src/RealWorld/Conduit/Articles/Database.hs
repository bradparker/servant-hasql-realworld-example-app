{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module RealWorld.Conduit.Articles.Database where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Functor.Contravariant (contramap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Result, Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import qualified Hasql.Session as Session
import Hasql.Session (QueryError)
import Hasql.Statement (Statement(Statement))
import Prelude hiding (all)
import RealWorld.Conduit.Articles.Article (Article(Article))
import RealWorld.Conduit.Users.Profile (Profile(Profile))
import Squeal.PostgreSQL
  ( (:::)
  , (:=>)
  , ColumnConstraint(Def, NoDef)
  , Expression
  , Grouping(Grouped, Ungrouped)
  , NP((:*))
  , NullityType(NotNull, Null)
  , PGType(..)
  , Query
  , SchemumType(Table)
  , SortExpression(Asc)
  , TableConstraint(ForeignKey, PrimaryKey)
  , (!)
  , (&)
  , (.==)
  -- , array
  , as
  , boolOr
  , count
  , false
  , from
  , fromNull
  , groupBy
  , ifThenElse
  , innerJoin
  , isNull
  , leftOuterJoin
  , limit
  , offset
  , orderBy
  , param
  , select
  , subquery
  , table
  , true
  , unsafeAggregate
  , unsafeBinaryOp
  , unsafeFunction
  , where_
  )
import Squeal.PostgreSQL.Render (renderSQL)

type Schema =
  '[ "users" ::: 'Table (
      '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
      '[ "id"   ::: 'Def :=> 'NotNull 'PGint8
       , "email" ::: 'NoDef :=> 'NotNull 'PGtext
       , "password" ::: 'NoDef :=> 'NotNull 'PGtext
       , "username" ::: 'NoDef :=> 'NotNull 'PGtext
       , "bio" ::: 'NoDef :=> 'NotNull 'PGtext
       , "image" ::: 'NoDef :=> 'Null 'PGtext
       ])
  , "articles" ::: 'Table (
      '[ "pk_articles" ::: 'PrimaryKey '["id"]
       , "fk_author__id" ::: 'ForeignKey '["author__id"] "users" '["id"]
       ] :=>
      '[ "id"   ::: 'Def :=> 'NotNull 'PGint8
       , "body" ::: 'NoDef :=> 'NotNull 'PGtext
       , "slug" ::: 'NoDef :=> 'NotNull 'PGtext
       , "title" ::: 'NoDef :=> 'NotNull 'PGtext
       , "description" ::: 'NoDef :=> 'NotNull 'PGtext
       , "author__id" ::: 'NoDef :=> 'NotNull 'PGint8
       , "created_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
       , "updated_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
       ])
  , "favorites" ::: 'Table (
      '[ "pk_favorites" ::: 'PrimaryKey '["user__id", "article__id"]
       , "fk_user__id" ::: 'ForeignKey '["user__id"] "users" '["id"]
       , "fk_article__id" ::: 'ForeignKey '["article__id"] "articles" '["id"]
       ] :=>
      '[ "user__id" ::: 'NoDef :=> 'NotNull 'PGint8
       , "article__id" ::: 'NoDef :=> 'NotNull 'PGint8
       ])
  , "follows" ::: 'Table (
      '[ "pk_follows" ::: 'PrimaryKey '["follower__id", "followee__id"]
       , "fk_follower__id" ::: 'ForeignKey '["follower__id"] "users" '["id"]
       , "fk_followee__id" ::: 'ForeignKey '["followee__id"] "users" '["id"]
       ] :=>
      '[ "follower__id" ::: 'NoDef :=> 'NotNull 'PGint8
       , "followee__id" ::: 'NoDef :=> 'NotNull 'PGint8
       ])
  , "article_tags" ::: 'Table (
      '[ "pk_article_tags" ::: 'PrimaryKey '["article__id", "tag__name"]
       , "fk_article_tag_article__id" ::: 'ForeignKey '["article__id"] "articls" '["id"]
       ] :=>
      '[ "article__id" ::: 'NoDef :=> 'NotNull 'PGint8
       , "tag__name" ::: 'NoDef :=> 'NotNull 'PGtext
       ])
  ]

arrayAgg
  :: Expression db from 'Ungrouped params (nullity ty)
  -> Expression db from ('Grouped bys) params ('NotNull ('PGvararray ty))
arrayAgg = unsafeAggregate "array_agg"

type ArticleRow =
  '[ "article_slug" ::: 'NotNull 'PGtext
   , "article_title" ::: 'NotNull 'PGtext
   , "article_description" ::: 'NotNull 'PGtext
   , "article_body" ::: 'NotNull 'PGtext
   , "article_tags_list" ::: 'NotNull ('PGvararray 'PGtext)
   , "article_created_at" ::: 'NotNull 'PGtimestamptz
   , "article_updated_at" ::: 'NotNull 'PGtimestamptz
   , "article_favorited" ::: 'NotNull 'PGbool
   , "article_favorites_count" ::: 'NotNull 'PGint8

   , "profile_username" ::: 'NotNull 'PGtext
   , "profile_bio" ::: 'NotNull 'PGtext
   , "profile_image" ::: 'Null 'PGtext
   , "profile_following" ::: 'NotNull 'PGbool
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
        `as` #profile_username :*
      #authors ! #bio
        `as` #profile_bio :*
      #authors ! #image
        `as` #profile_image :*
      boolOr
        (ifThenElse
          (isNull (param @1))
          false
          (fromNull false (#follows ! #follower__id .== param @1)))
        `as` #profile_following)

      (from (table (#articles `as` #articles)
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
          #authors ! #image))

arrayOverlap ::
  Expression db from grouping params (nullity ('PGvararray ty)) ->
  Expression db from grouping params (nullity ('PGvararray ty)) ->
  Expression db from grouping params (nullity 'PGbool)
arrayOverlap = unsafeBinaryOp "&&"

arrayContains ::
  Expression db from grouping params (nullity ('PGvararray ty)) ->
  Expression db from grouping params (nullity ('PGvararray ty)) ->
  Expression db from grouping params (nullity 'PGbool)
arrayContains = unsafeBinaryOp "@>"

arrayCardinality ::
  Expression db from grouping params (nullity ('PGvararray ty)) ->
  Expression db from grouping params (nullity 'PGint8)
arrayCardinality = unsafeFunction "cardinality"

selectFilteredArticlesQuery ::
     Word64
  -> Word64
  -> Query
       Schema
        '[ 'Null 'PGint8
         , 'NotNull ('PGvararray 'PGtext)
         , 'NotNull ('PGvararray 'PGtext)
         ]
      ArticleRow
selectFilteredArticlesQuery lim off =
  select
    ( #article_slug :*
      #article_title :*
      #article_description :*
      #article_body :*
      #article_tags_list :*
      #article_created_at :*
      #article_updated_at :*
      #article_favorited :*
      #article_favorites_count :*

      #profile_username :*
      #profile_bio :*
      #profile_image :*
      #profile_following )

    (from
        (subquery
          (selectArticlesQuery
            `as` #filtered_articles))
      & where_
        (ifThenElse
          (arrayCardinality (param @2) .== 0)
          true
          (arrayOverlap
            (param @2)
            (#filtered_articles ! #article_tags_list)))
      -- & where_
      --   (ifThenElse
      --     (arrayCardinality (param @3) .== 0)
      --     true
      --     (arrayContains
      --       (param @3)
      --       (array [#filtered_articles ! #article_tags_list])))
      & orderBy [#filtered_articles ! #article_created_at & Asc]
      & limit lim
      & offset off)

selectFeedArticlesQuery ::
     Word64
  -> Word64
  -> Query
       Schema
        '[ 'Null 'PGint8
         , 'NotNull ('PGvararray 'PGtext)
         , 'NotNull ('PGvararray 'PGtext)
         ]
      ArticleRow
selectFeedArticlesQuery lim off =
  select
    ( #article_slug :*
      #article_title :*
      #article_description :*
      #article_body :*
      #article_tags_list :*
      #article_created_at :*
      #article_updated_at :*
      #article_favorited :*
      #article_favorites_count :*

      #profile_username :*
      #profile_bio :*
      #profile_image :*
      #profile_following )
    (from
        (subquery
          (selectFilteredArticlesQuery lim off
            `as` #filtered_articles))
      & where_
        (#filtered_articles ! #profile_following))

extendStatement :: (ByteString -> ByteString) -> (Params a -> Params a') -> (Result b -> Result b') -> Statement a b -> Statement a' b'
extendStatement transSql transEnc transDec (Statement sql enc dec prep) =
  Statement (transSql sql) (transEnc enc) (transDec dec) prep

profileRow :: Row Profile
profileRow =
  Profile
    <$> Decoders.column Decoders.text
    <*> Decoders.column Decoders.text
    <*> Decoders.nullableColumn Decoders.text
    <*> Decoders.column Decoders.bool

tagsValue :: Decoders.Value (Set Text)
tagsValue =
  Set.fromList <$>
    Decoders.array
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

data ArticleQuery = ArticleQuery
  { tagNames :: Set Text
  , usernames :: Set Text
  }

foldableOfTextEncoder :: Foldable t => Encoders.Value (t Text)
foldableOfTextEncoder =
  Encoders.array (Encoders.dimension foldl' (Encoders.element Encoders.text))

articleQueryEncoder :: Params ArticleQuery
articleQueryEncoder =
  contramap tagNames (Encoders.param foldableOfTextEncoder) <>
  contramap usernames (Encoders.param foldableOfTextEncoder)

selectArticles :: Statement (Maybe Int) [Article]
selectArticles = Statement sql encoder decoder True
  where
    sql = renderSQL selectArticlesQuery
    encoder = contramap (fmap fromIntegral) (Encoders.nullableParam Encoders.int8)
    decoder = Decoders.rowList articleRow

selectFilteredArticles :: Word64 -> Word64 -> Statement (Maybe Int, ArticleQuery) [Article]
selectFilteredArticles lim off = Statement sql encoder decoder True
  where
    sql = renderSQL (selectFilteredArticlesQuery lim off)
    encoder =
      contramap (fmap fromIntegral . fst) (Encoders.nullableParam Encoders.int8) <>
      contramap snd articleQueryEncoder
    decoder = Decoders.rowList articleRow

selectFeedArticles :: Word64 -> Word64 -> Statement (Maybe Int, ArticleQuery) [Article]
selectFeedArticles lim off = Statement sql encoder decoder True
  where
    sql = renderSQL (selectFeedArticlesQuery lim off)
    encoder =
      contramap (fmap fromIntegral . fst) (Encoders.nullableParam Encoders.int8) <>
      contramap snd articleQueryEncoder
    decoder = Decoders.rowList articleRow

selectArticleQuery :: Query Schema '[ 'Null 'PGint8, 'NotNull 'PGtext ] ArticleRow
selectArticleQuery =
  select (
    #article_slug :*
    #article_title :*
    #article_description :*
    #article_body :*
    #article_tags_list :*
    #article_created_at :*
    #article_updated_at :*
    #article_favorited :*
    #article_favorites_count :*

    #profile_username :*
    #profile_bio :*
    #profile_image :*
    #profile_following
  ) (
    from (subquery (selectArticlesQuery `as` #filtered_articles))
    & where_
      (#filtered_articles ! #article_slug .== param @2)
    )

selectArticle :: Statement (Maybe Int, Text) (Maybe Article)
selectArticle = Statement sql encoder decoder True
  where
    sql = renderSQL selectArticleQuery
    encoder =
      contramap ((fromIntegral <$>) . fst) (Encoders.nullableParam Encoders.int8) <>
      contramap snd (Encoders.param Encoders.text)
    decoder =
      Decoders.rowMaybe articleRow

defaultArticleQuery :: ArticleQuery
defaultArticleQuery =
  ArticleQuery Set.empty Set.empty

all :: Maybe Int -> Word64 -> Word64 -> ArticleQuery -> Connection -> IO (Either QueryError [Article])
all currentUser lim off query =
  Session.run
    (Session.statement (currentUser, query) (selectFilteredArticles lim off))

feed :: Maybe Int -> Word64 -> Word64 -> ArticleQuery -> Connection -> IO (Either QueryError [Article])
feed currentUser lim off query =
  Session.run
    (Session.statement (currentUser, query) (selectFeedArticles lim off))

find :: Maybe Int -> Text -> Connection -> IO (Either QueryError (Maybe Article))
find currentUser slug =
  Session.run
    (Session.statement (currentUser, slug) selectArticle)
