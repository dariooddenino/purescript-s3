module Network.AWS.Options where

import Data.Options (Option(), Options(), opt)
import Data.Monoid (mempty)

foreign import data S3Opt :: *

region :: Option S3Opt String
region = opt "region"

accessKeyId :: Option S3Opt String
accessKeyId = opt "accessKeyId"

secretAccessKey :: Option S3Opt String
secretAccessKey = opt "secretAccessKey"

type S3Options = Options S3Opt

defaultOptions :: S3Options
defaultOptions = mempty

-- params :: map {completely freeform}
-- endpoint :: String
-- accessKeyId :: String
-- secretAccessKey :: String
-- sessionToken :: AWS.Credentials
-- credentials :: AWS.Credentials
-- credentialProvider :: AWS.CredentialProviderChain
-- region :: String
-- maxRetries :: Integer
-- MaxRedirects :: Integer
-- sslEnabled :: Boolean
-- paramValidation :: Boolean | map { min :: Boolean, max :: Boolean, pattern :: Boolean, enum :: Boolean}
-- computeChecksums :: Boolean
-- convertResponseTypes :: Boolean
-- correctClockSkew :: Boolean
-- s3ForcePathStyle :: Boolean
-- s3BucketEndpoint :: Boolean
-- s3DisabledBodySigning :: Boolean
-- retryDelayOptions :: map { base :: Int, customBackoff :: function }
-- httpOptions :: map { proxy :: String, agent :: http.Agent | https.Agent, timeout :: Int, xhrAsync :: Boolean, xhrWithCredentials :: Boolean }
-- apiVersion :: String
-- logger :: object that responds to .write or .log
-- systemClockOffset :: Int
-- signatureVersion :: String
-- signatureCache :: Boolean
