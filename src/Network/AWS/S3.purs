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
import Network.AWS.S3.Bucket
import Control.Monad.Aff
import Data.Foldable
import Network.AWS.S3.Common (S3, S3Obj)

foreign import _init
  :: Foreign
  -> S3Obj

init :: S3Options -> S3Obj
init = _init <<< options

foreign import traceAny :: forall e a. a -> Eff (console :: CONSOLE | e) Unit

-- just to see if anything gets printed
test' = void $ launchAff $ do
  buckets <- listBuckets s3
  case buckets of
    Left _ -> liftEff $ traceAny "no"
    Right (BucketResponse b) -> liftEff $ traceAny $ foldMap show b.buckets
  --liftEff $ traceAny buckets
  where
    s3 = init $ defaultOptions
