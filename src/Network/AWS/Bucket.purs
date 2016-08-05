module Network.AWS.S3.Bucket where

import Prelude
import Network.AWS.S3.Common (readDate, S3, S3Obj)
import Data.Foreign (F, Foreign(), ForeignError)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Index (prop)
import Data.JSDate
import Control.Monad.Aff
import Control.Monad.Eff
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Either (Either)
import Control.Monad.Eff.Exception
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)

newtype Bucket = Bucket
              { name :: String
              , creationDate :: Either String DateTime
              }

instance bucketIsForeign :: IsForeign Bucket where
  read value = do
    name <- readProp "Name" value
    creationDate <- readProp "CreationDate" value
    pure $ Bucket { name, creationDate: readDate creationDate }

instance showBucket :: Show Bucket where
  show (Bucket { name, creationDate }) = "Bucket: " <> show name <> " " <> show creationDate

newtype BucketResponse = BucketResponse
  { buckets :: Array Bucket
  , owner :: { displayName :: String
             , id :: String
             }
  }

instance bucketReponseIsForeign :: IsForeign BucketResponse where
  read value = do
    buckets <- readProp "Buckets" value
    displayName <- value # (prop "Owner" >=> readProp "DisplayName")
    id <- value # (prop "Owner" >=> readProp "ID")
    pure $ BucketResponse { buckets, owner: { displayName, id } }

foreign import _listBuckets
  :: forall eff
   . Fn3 S3Obj
     (Error -> Eff (s3 :: S3 | eff) Unit)
     (Foreign -> Eff (s3 :: S3 | eff) Unit)
     (Eff (s3 :: S3 | eff) Unit)

listBuckets
  :: forall eff
   . S3Obj
  -> Aff (s3 :: S3 | eff) (Either ForeignError BucketResponse)
listBuckets s3 = makeAff (\error success -> runFn3 _listBuckets s3 error (success <<< read))
-- Here I have both an error callback, and a success callback that is an Either Error Response.
