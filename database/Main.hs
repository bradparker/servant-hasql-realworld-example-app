module Main
  ( main
  ) where

import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Network.URI (URI)
import qualified Network.URI as URI
import RealWorld.Conduit.Database.Schema
  ( ArticlesTable
  , FavoritesTable
  , FollowsTable
  , Schema
  , TagsTable
  , UsersTable
  , CommentsTable
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
import System.Environment (getArgs, getEnv)
import System.Exit (die)
import System.Process (readProcess)

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
            notNullable int8 `as` #author_id :*
            notNullable timestampWithTimeZone `as` #created_at :*
            notNullable timestampWithTimeZone `as` #updated_at )
          ( primaryKey #id `as` #pk_articles :*
            foreignKey #author_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_author_id )
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
          ( notNullable int8 `as` #user_id :*
            notNullable int8 `as` #article_id )
          ( primaryKey (#user_id  :* #article_id) `as` #pk_favorites :*
            foreignKey #user_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_user_id :*
            foreignKey #article_id #articles #id
              OnDeleteCascade OnUpdateCascade `as` #fk_article_id )
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
          ( notNullable int8 `as` #follower_id :*
            notNullable int8 `as` #followee_id )
          ( primaryKey (#follower_id  :* #followee_id) `as` #pk_follows :*
            foreignKey #follower_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_follower_id :*
            foreignKey #followee_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_followee_id )
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
        , "tags" ::: TagsTable
        ]
makeTags =
  Migration
    { name = "make tags table"
    , up =
        void . define $
        createTable
          #tags
          ( notNullable int8 `as` #article_id :*
            notNullable text `as` #name )
          ( primaryKey (#article_id  :* #name) `as` #pk_tags :*
            foreignKey #article_id #articles #id
              OnDeleteCascade OnUpdateCascade `as` #fk_tag_article_id )
    , down = void . define $ dropTable #tags
    }

makeComments
  :: Migration
       IO
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        , "follows" ::: FollowsTable
        , "tags" ::: TagsTable
        ]
       '[ "users" ::: UsersTable
        , "articles" ::: ArticlesTable
        , "favorites" ::: FavoritesTable
        , "follows" ::: FollowsTable
        , "tags" ::: TagsTable
        , "comments" ::: CommentsTable
        ]
makeComments =
  Migration
    { name = "make comments table"
    , up =
        void . define $
        createTable
          #comments
          ( serial8 `as` #id :*
            notNullable text `as` #body :*
            notNullable int8 `as` #author_id :*
            notNullable int8 `as` #article_id :*
            notNullable timestampWithTimeZone `as` #created_at :*
            notNullable timestampWithTimeZone `as` #updated_at )
          ( primaryKey #id `as` #pk_comments :*
            foreignKey #author_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_comment_author_id :*
            foreignKey #article_id #articles #id
              OnDeleteCascade OnUpdateCascade `as` #fk_comment_article_id )
    , down = void . define $ dropTable #comments
    }

migrations :: AlignedList (Migration IO) '[] Schema
migrations =
  makeUsers
    :>> makeArticles
    :>> makeFavorites
    :>> makeFollows
    :>> makeTags
    :>> makeComments
    :>> Done

migrateAll :: ByteString -> IO ()
migrateAll databaseUrl = do
  awaitDatabaseReady =<< parseDatabaseUrl databaseUrl
  putStrLn "Applying all migrations"
  withConnection databaseUrl $ migrateUp migrations

rollbackAll :: ByteString -> IO ()
rollbackAll databaseUrl = do
  awaitDatabaseReady =<< parseDatabaseUrl databaseUrl
  putStrLn "Rolling back all migrations"
  withConnection databaseUrl $ migrateDown migrations

initDB :: IO ()
initDB =
  putStrLn =<< readProcess "pg_ctl" ["-w", "init"] ""

startDB :: IO ()
startDB =
  putStrLn =<< readProcess "pg_ctl" ["-w", "-l", ".dev/logs/database.log", "start"] ""

stopDB :: IO ()
stopDB =
  putStrLn =<< readProcess "pg_ctl" ["-w", "stop"] ""

awaitDatabaseReady :: URI -> IO ()
awaitDatabaseReady uri = void $ readProcess
  "pg_isready"
  ["-h", databaseHost uri, "-p", databasePort uri]
  ""

databaseExists :: String -> IO Bool
databaseExists dbName = do
  databases <- lines <$> readProcess
    "psql"
    [ "postgres"
    , "--tuples-only"
    , "--no-align"
    , "--command"
    , "SELECT datname FROM pg_database;"
    ]
    ""
  pure $ dbName `elem` databases

parseDatabaseUrl :: ByteString -> IO URI
parseDatabaseUrl databaseUrl =
  case URI.parseURI (ByteString.unpack databaseUrl) of
    Nothing  -> die $ "Malformed URL: " ++ ByteString.unpack databaseUrl
    Just uri -> pure uri

databaseName :: URI -> String
databaseName = drop 1 . URI.uriPath

databaseHost :: URI -> String
databaseHost = maybe "" URI.uriRegName . URI.uriAuthority

databasePort :: URI -> String
databasePort = drop 1 . maybe "" URI.uriPort . URI.uriAuthority

createDB :: ByteString -> IO ()
createDB databaseUrl =  do
  uri <- parseDatabaseUrl databaseUrl
  awaitDatabaseReady uri
  exists <- databaseExists $ databaseName uri
  when exists $ putStrLn $ "Database " ++  databaseName uri ++" exists. Skipping."
  unless exists $ do
    putStrLn $ "Creating database " ++ databaseName uri
    void $ readProcess
      "createdb"
      [ "-h"
      , databaseHost uri
      , "-p"
      , databasePort uri
      , databaseName uri
      ]
      ""
    putStrLn $ "Created " ++ databaseName uri

dropDB :: ByteString -> IO ()
dropDB databaseUrl =  do
  uri <- parseDatabaseUrl databaseUrl
  awaitDatabaseReady uri
  exists <- databaseExists $ databaseName uri
  unless exists $ putStrLn $ "Database " ++  databaseName uri ++" doesn't exist. Skipping."
  when exists $ do
    putStrLn $ "Dropping database " ++ databaseName uri
    void $ readProcess
      "dropdb"
      [ "-h"
      , databaseHost uri
      , "-p"
      , databasePort uri
      , databaseName uri
      ]
      ""
    putStrLn $ "Dropped " ++ databaseName uri

setupDB :: ByteString -> IO ()
setupDB databaseUrl = do
  createDB databaseUrl
  migrateAll databaseUrl

resetDB :: ByteString -> IO ()
resetDB databaseUrl = do
  dropDB databaseUrl
  setupDB databaseUrl

data Command
  = Init
  | Start
  | Stop
  | Create
  | Drop
  | Migrate
  | Rollback
  | Setup
  | Reset

parseCommand :: String -> Maybe Command
parseCommand "init" = Just Init
parseCommand "start" = Just Start
parseCommand "stop" = Just Stop
parseCommand "create" = Just Create
parseCommand "drop" = Just Drop
parseCommand "migrate" = Just Migrate
parseCommand "rollback" = Just Rollback
parseCommand "setup" = Just Setup
parseCommand "reset" = Just Reset
parseCommand _ = Nothing

main :: IO ()
main = do
  databaseUrl <- ByteString.pack <$> getEnv "DATABASE_URL"
  command <- (parseCommand =<<) . listToMaybe <$> getArgs
  case command of
    Just Init -> initDB
    Just Start -> startDB
    Just Stop -> stopDB
    Just Create -> createDB databaseUrl
    Just Drop -> dropDB databaseUrl
    Just Migrate -> migrateAll databaseUrl
    Just Rollback -> rollbackAll databaseUrl
    Just Setup -> setupDB databaseUrl
    Just Reset -> resetDB databaseUrl
    Nothing -> die "ERROR: Unknown command"
