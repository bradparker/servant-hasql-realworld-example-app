module RealWorld.Conduit.Database.Schema
  ( Schema
  , UsersTable
  , ArticlesTable
  , FavoritesTable
  , FollowsTable
  , TagsTable
  , CommentsTable
  ) where

import Squeal.PostgreSQL.Extended
  ( (:::)
  , (:=>)
  , ColumnConstraint(Def, NoDef)
  , NullityType(NotNull, Null)
  , PGType(..)
  , SchemumType(Table)
  , TableConstraint(ForeignKey, PrimaryKey)
  )

type UsersTable =
  'Table (
    '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
    '[ "id"   ::: 'Def :=> 'NotNull 'PGint8
     , "email" ::: 'NoDef :=> 'NotNull 'PGtext
     , "password" ::: 'NoDef :=> 'NotNull 'PGtext
     , "username" ::: 'NoDef :=> 'NotNull 'PGtext
     , "bio" ::: 'NoDef :=> 'NotNull 'PGtext
     , "image" ::: 'NoDef :=> 'Null 'PGtext
     ])

type ArticlesTable =
  'Table (
    '[ "pk_articles" ::: 'PrimaryKey '["id"]
     , "fk_author_id" ::: 'ForeignKey '["author_id"] "users" '["id"]
     ] :=>
    '[ "id"   ::: 'Def :=> 'NotNull 'PGint8
     , "body" ::: 'NoDef :=> 'NotNull 'PGtext
     , "slug" ::: 'NoDef :=> 'NotNull 'PGtext
     , "title" ::: 'NoDef :=> 'NotNull 'PGtext
     , "description" ::: 'NoDef :=> 'NotNull 'PGtext
     , "author_id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "created_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
     , "updated_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
     ])

type FavoritesTable =
  'Table (
    '[ "pk_favorites" ::: 'PrimaryKey '["user_id", "article_id"]
     , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
     , "fk_article_id" ::: 'ForeignKey '["article_id"] "articles" '["id"]
     ] :=>
    '[ "user_id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "article_id" ::: 'NoDef :=> 'NotNull 'PGint8
     ])

type FollowsTable =
  'Table (
    '[ "pk_follows" ::: 'PrimaryKey '["follower_id", "followee_id"]
     , "fk_follower_id" ::: 'ForeignKey '["follower_id"] "users" '["id"]
     , "fk_followee_id" ::: 'ForeignKey '["followee_id"] "users" '["id"]
     ] :=>
    '[ "follower_id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "followee_id" ::: 'NoDef :=> 'NotNull 'PGint8
     ])

type TagsTable =
  'Table (
    '[ "pk_tags" ::: 'PrimaryKey '["article_id", "name"]
     , "fk_tag_article_id" ::: 'ForeignKey '["article_id"] "articles" '["id"]
     ] :=>
    '[ "article_id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "name" ::: 'NoDef :=> 'NotNull 'PGtext
     ])

type CommentsTable =
  'Table (
    '[ "pk_comments" ::: 'PrimaryKey '["id"]
     , "fk_comment_author_id" ::: 'ForeignKey '["author_id"] "users" '["id"]
     , "fk_comment_article_id" ::: 'ForeignKey '["article_id"] "articles" '["id"]
     ] :=>
    '[ "id" ::: 'Def :=> 'NotNull 'PGint8
     , "body" ::: 'NoDef :=> 'NotNull 'PGtext
     , "author_id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "article_id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "created_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
     , "updated_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
     ])

type Schema =
  '[ "users" ::: UsersTable
   , "articles" ::: ArticlesTable
   , "favorites" ::: FavoritesTable
   , "follows" ::: FollowsTable
   , "tags" ::: TagsTable
   , "comments" ::: CommentsTable
   ]
