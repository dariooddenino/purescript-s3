module Network.AWS.S3.Common where

import Data.JSDate (JSDate, toDateTime)
import Data.Foreign (F, Foreign())
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(..))
import Data.DateTime (DateTime)
import Data.Either (Either(..))

foreign import data S3 :: !
foreign import data S3Obj :: *

readDate :: Foreign -> Maybe DateTime
readDate d = let
  res = (read d :: F JSDate) in
  case res of
    Left _ -> Nothing
    Right jsdate -> toDateTime jsdate
