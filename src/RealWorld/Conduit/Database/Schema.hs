module RealWorld.Conduit.Database.Schema
  ( Schema
  , UsersTable
  , ArticlesTable
  , FavoritesTable
  , FollowsTable
  , TagsTable
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

type FavoritesTable =
  'Table (
    '[ "pk_favorites" ::: 'PrimaryKey '["user__id", "article__id"]
     , "fk_user__id" ::: 'ForeignKey '["user__id"] "users" '["id"]
     , "fk_article__id" ::: 'ForeignKey '["article__id"] "articles" '["id"]
     ] :=>
    '[ "user__id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "article__id" ::: 'NoDef :=> 'NotNull 'PGint8
     ])

type FollowsTable =
  'Table (
    '[ "pk_follows" ::: 'PrimaryKey '["follower__id", "followee__id"]
     , "fk_follower__id" ::: 'ForeignKey '["follower__id"] "users" '["id"]
     , "fk_followee__id" ::: 'ForeignKey '["followee__id"] "users" '["id"]
     ] :=>
    '[ "follower__id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "followee__id" ::: 'NoDef :=> 'NotNull 'PGint8
     ])

type TagsTable =
  'Table (
    '[ "pk_article_tags" ::: 'PrimaryKey '["article__id", "tag__name"]
     , "fk_article_tag_article__id" ::: 'ForeignKey '["article__id"] "articles" '["id"]
     ] :=>
    '[ "article__id" ::: 'NoDef :=> 'NotNull 'PGint8
     , "tag__name" ::: 'NoDef :=> 'NotNull 'PGtext
     ])

type Schema =
  '[ "users" ::: UsersTable
   , "articles" ::: ArticlesTable
   , "favorites" ::: FavoritesTable
   , "follows" ::: FollowsTable
   , "article_tags" ::: TagsTable
   ]
