module Main
  ( main
  ) where

import Data.Maybe (listToMaybe)
import System.Exit (die)
import System.Process (readProcess)
import qualified Data.ByteString.Char8 as BS
import Data.Functor (void)
import RealWorld.Conduit.Database.Schema
  ( ArticlesTable
  , FavoritesTable
  , FollowsTable
  , Schema
  , TagsTable
  , UsersTable
  )
import Squeal.PostgreSQL.Extended
  ( (:::)
  , AlignedList((:>>), Done)
  , NP((:*))
  , OnDeleteClause(OnDeleteCascade)
  , OnUpdateClause(OnUpdateCascade)
  , as
  , createTable
  , define
  , dropTable
  , foreignKey
  , int8
  , notNullable
  , nullable
  , primaryKey
  , serial8
  , text
  , timestampWithTimeZone
  , withConnection
  )
import Squeal.PostgreSQL.Migration
  ( Migration(Migration, down, name, up)
  , migrateDown
  , migrateUp
  )
import System.Environment (getEnv, getArgs)

makeUsers :: Migration IO '[] '["users" ::: UsersTable]
makeUsers =
  Migration
    { name = "make users table"
    , up =
        void . define $
        createTable
          #users
          ( serial8 `as` #id :*
            notNullable text `as` #email :*
            notNullable text `as` #password :*
            notNullable text `as` #username :*
            notNullable text `as` #bio :*
            nullable text `as` #image
          )
          (primaryKey #id `as` #pk_users)
    , down = void . define $ dropTable #users
    }

makeArticles
  :: Migration
       IO
       '["users" ::: UsersTable]
       '["users" ::: UsersTable, "articles" ::: ArticlesTable]
makeArticles =
  Migration
    { name = "make articles table"
    , up =
        void . define $
        createTable
          #articles
          ( serial8 `as` #id :*
            notNullable text `as` #body :*
            notNullable text `as` #slug :*
            notNullable text `as` #title :*
            notNullable text `as` #description :*
            notNullable int8 `as` #author__id :*
            notNullable timestampWithTimeZone `as` #created_at :*
            notNullable timestampWithTimeZone `as` #updated_at )
          ( primaryKey #id `as` #pk_articles :*
            foreignKey #author__id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_author__id )
    , down = void . define $ dropTable #articles
    }

makeFavorites
  :: Migration
       IO
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        ]
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        ]
makeFavorites =
  Migration
    { name = "make favorites table"
    , up =
        void . define $
        createTable
          #favorites
          ( notNullable int8 `as` #user__id :*
            notNullable int8 `as` #article__id )
          ( primaryKey (#user__id  :* #article__id) `as` #pk_favorites :*
            foreignKey #user__id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_user__id :*
            foreignKey #article__id #articles #id
              OnDeleteCascade OnUpdateCascade `as` #fk_article__id )
    , down = void . define $ dropTable #favorites
    }

makeFollows
  :: Migration
       IO
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        ]
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        , "follows" ::: FollowsTable
        ]
makeFollows =
  Migration
    { name = "make follows table"
    , up =
        void . define $
        createTable
          #follows
          ( notNullable int8 `as` #follower__id :*
            notNullable int8 `as` #followee__id )
          ( primaryKey (#follower__id  :* #followee__id) `as` #pk_follows :*
            foreignKey #follower__id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_follower__id :*
            foreignKey #followee__id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_followee__id )
    , down = void . define $ dropTable #follows
    }

makeTags
  :: Migration
       IO
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        , "follows" ::: FollowsTable
        ]
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        , "follows" ::: FollowsTable
        , "article_tags" ::: TagsTable
        ]
makeTags =
  Migration
    { name = "make article_tags table"
    , up =
        void . define $
        createTable
          #article_tags
          ( notNullable int8 `as` #article__id :*
            notNullable text `as` #tag__name )
          ( primaryKey (#article__id  :* #tag__name) `as` #pk_article_tags :*
            foreignKey #article__id #articles #id
              OnDeleteCascade OnUpdateCascade `as` #fk_article_tag_article__id )
    , down = void . define $ dropTable #article_tags
    }

migrations :: AlignedList (Migration IO) '[] Schema
migrations =
  makeUsers
    :>> makeArticles
    :>> makeFavorites
    :>> makeFollows
    :>> makeTags
    :>> Done

migrateAll :: BS.ByteString -> IO ()
migrateAll databaseUrl = withConnection databaseUrl $ migrateUp migrations

rollbackAll :: BS.ByteString -> IO ()
rollbackAll databaseUrl = withConnection databaseUrl $ migrateDown migrations

initDB :: IO ()
initDB =
  putStrLn =<< readProcess "pg_ctl" ["-w", "init"] ""

startDB :: IO ()
startDB =
  putStrLn =<< readProcess "pg_ctl" ["-w", "-l", "logs/database.log", "start"] ""

stopDB :: IO ()
stopDB =
  putStrLn =<< readProcess "pg_ctl" ["-w", "stop"] ""

createDB :: BS.ByteString -> IO ()
createDB _ =
  putStrLn =<< readProcess "createdb" ["conduit-hasql"] ""

dropDB :: BS.ByteString -> IO ()
dropDB _ =
  putStrLn =<< readProcess "dropdb" ["conduit-hasql"] ""

data Command
  = Init
  | Start
  | Stop
  | Create
  | Drop
  | Migrate
  | Rollback

parseCommand :: String -> Maybe Command
parseCommand "init" = Just Init
parseCommand "start" = Just Start
parseCommand "stop" = Just Stop
parseCommand "create" = Just Create
parseCommand "drop" = Just Drop
parseCommand "migrate" = Just Migrate
parseCommand "rollback" = Just Rollback
parseCommand _ = Nothing

main :: IO ()
main = do
  databaseUrl <- BS.pack <$> getEnv "DATABASE_URL"
  command <- (parseCommand =<<) . listToMaybe <$> getArgs
  case command of
    Just Init -> initDB
    Just Start -> startDB
    Just Stop -> stopDB
    Just Create -> createDB databaseUrl
    Just Drop -> dropDB databaseUrl
    Just Migrate -> migrateAll databaseUrl
    Just Rollback -> rollbackAll databaseUrl
    Nothing -> die "ERROR: Unknown command"
