module Network.AWS.S3 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception
import Data.Either
import Data.Maybe
import Control.Monad.Trans
import Data.Function.Uncurried (Fn3, runFn3)
import Network.AWS.Options
import Data.Options ((:=), options, opt)
import Data.Foreign (Foreign())
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

foreign import data S3 :: !
foreign import data S3Obj :: *

foreign import _init
  :: Foreign
  -> S3Obj

init :: S3Options -> S3Obj
init = _init <<< options

data Bucket = String
data Buckets = Array Bucket

instance decodeJsonBucket :: DecodeJson Bucket where
  decodeJson json = do
    obj <- decodeJson json
    buckets <- obj .? "Buckets"

type Buckets = { "Buckets" :: Array { "Name" :: String, "CreationDate" :: String }
         , "Owner" :: { "DisplayName" :: String, "ID" :: String } }

foreign import _listBuckets
  :: forall eff
   . Fn3 S3Obj
     (Error -> Eff (s3 :: S3 | eff) Unit)
     (Buckets -> Eff (s3 :: S3 | eff) Unit)
     (Eff (s3 :: S3 | eff) Unit)

listBuckets
  :: forall eff
   . S3Obj
  -> Aff (s3 :: S3 | eff) Buckets
listBuckets s3 = makeAff (\error success -> runFn3 _listBuckets s3 error success)

foreign import traceAny :: forall e a. a -> Eff (console :: CONSOLE | e) Unit

-- tried every possible combination with launchAff / runAff...
-- no output
test' = void $ launchAff $ do
  buckets <- listBuckets s3
  liftEff $ traceAny buckets
  where
    s3 = init $ defaultOptions
 
