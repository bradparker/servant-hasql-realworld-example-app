module RealWorld.Conduit.Users.Database
  ( selectProfilesQuery
  , profileRow
  , findProfile
  , ProfileRow
  ) where

import Control.Lens (_1, _2, view)
import Data.Text (Text)
import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import qualified Hasql.Session as Session
import Data.Functor.Contravariant (contramap)
import Hasql.Statement (Statement(Statement))
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Data.Function ((&))
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import RealWorld.Conduit.Database.Schema (Schema)
import RealWorld.Conduit.Users.Profile (Profile(Profile))
import Squeal.PostgreSQL.Extended
  ( (:::)
  , NP((:*))
  , NullityType(NotNull, Null)
  , PGType(..)
  , Query
  , (!)
  , (.==)
  , as
  , boolOr
  , selectStar
  , subquery
  , false
  , from
  , fromNull
  , groupBy
  , ifThenElse
  , isNull
  , where_
  , renderSQL
  , leftOuterJoin
  , param
  , select
  , table
  )

profileRow :: Row Profile
profileRow =
  Profile
    <$> Decoders.column Decoders.text
    <*> Decoders.column Decoders.text
    <*> Decoders.nullableColumn Decoders.text
    <*> Decoders.column Decoders.bool

type ProfileRow =
  '[ "username" ::: 'NotNull 'PGtext
   , "bio" ::: 'NotNull 'PGtext
   , "image" ::: 'Null 'PGtext
   , "following" ::: 'NotNull 'PGbool
   ]

selectProfilesQuery :: Query Schema ('Null 'PGint8 : params) ProfileRow
selectProfilesQuery =
  select
    ( #profiles ! #username
        `as` #username :*
      #profiles ! #bio
        `as` #bio :*
      #profiles ! #image
        `as` #image :*
      boolOr
        (ifThenElse
          (isNull (param @1))
          false
          (fromNull false (#follows ! #follower_id .== param @1)))
        `as` #following )
  $ from (table (#users `as` #profiles)
      & leftOuterJoin
          (table (#follows `as` #follows))
          (#profiles ! #id .== #follows ! #followee_id))
  & groupBy
      ( #profiles ! #username :*
        #profiles ! #bio :*
        #profiles ! #image )

currentUserIdEncoder :: Params (Maybe Int)
currentUserIdEncoder =
  contramap (fmap fromIntegral) (Encoders.nullableParam Encoders.int8)

selectProfileQuery
  :: Query Schema '[ 'Null 'PGint8, 'NotNull 'PGtext] ProfileRow
selectProfileQuery =
  selectStar
    $ from (subquery (selectProfilesQuery `as` #profiles))
    & where_ (#profiles ! #username .== param @2)

selectProfile :: Statement (Maybe Int, Text) (Maybe Profile)
selectProfile = Statement sql encoder decoder True
  where
    sql = renderSQL selectProfileQuery
    encoder =
      contramap (view _1) currentUserIdEncoder <>
      contramap (view _2) (Encoders.param Encoders.text)
    decoder = Decoders.rowMaybe profileRow

findProfile
  :: Maybe Int -> Text -> Connection -> IO (Either QueryError (Maybe Profile))
findProfile currentUser username =
  Session.run (Session.statement (currentUser, username) selectProfile)
