module Network.AWS.S3.Bucket where

import Prelude
import Network.AWS.S3.Common (readDate, S3, S3Obj)
import Data.Foreign (F, Foreign(), ForeignError)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Index as I
import Data.Foreign.Lens (prop, array)
import Data.Lens (FoldP(), (^..), traversed, to)
import Data.JSDate
import Control.Monad.Aff
import Control.Monad.Eff
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Either (Either)
import Control.Monad.Eff.Exception
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)
import Data.Traversable (traverse)
import Data.Monoid (class Monoid)
import Data.List (List)


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

bucket :: forall r. Monoid r => FoldP r Foreign Bucket
bucket = to read <<< traversed

buckets :: forall r. Monoid r => FoldP r Foreign Bucket
buckets = prop "Buckets"
      <<< array
      <<< traversed
      <<< bucket

foreign import _listBuckets
  :: forall eff
   . Fn3 S3Obj
     (Error -> Eff (s3 :: S3 | eff) Unit)
     (Foreign -> Eff (s3 :: S3 | eff) Unit)
     (Eff (s3 :: S3 | eff) Unit)

listBuckets
  :: forall eff
   . S3Obj
  -> Aff (s3 :: S3 | eff) (List Bucket)
listBuckets s3 = makeAff (\error success -> runFn3 _listBuckets s3 error (success <<< (\r -> r ^.. buckets)))

