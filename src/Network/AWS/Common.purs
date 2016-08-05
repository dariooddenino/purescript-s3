module Network.AWS.S3.Common where

import Prelude (($), const, bind, show)
import Data.JSDate (JSDate, toDateTime)
import Data.Foreign (F, Foreign())
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(..), maybe)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)

foreign import data S3 :: !
foreign import data S3Obj :: *

readDate :: Foreign -> Either String DateTime
readDate d = do
  res <- either (\err -> Left (show err)) Right (read d :: F JSDate)
  maybe (Left "Error converting date") Right $ toDateTime res
