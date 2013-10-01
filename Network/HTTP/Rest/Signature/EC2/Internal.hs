module Network.HTTP.Rest.Signature.EC2.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Digest.Pure.SHA as SHA
import Data.Monoid
import qualified Network.HTTP.Types as HTTP

import Network.HTTP.QueryString

type Method = ByteString
type Endpoint = ByteString
type Path = ByteString
type SecretKey = ByteString
data SignatureMethod = HmacSHA256

-- | Make a string for making signature.
--
-- >>> stringToSign "GET" "ec2.amazonaws.com" "/" [("key1", "value1"), ("key2", "value2")]
-- "GET\nec2.amazonaws.com\n/\nkey1=value1&key2=value2"
stringToSign :: Method -> Endpoint -> Path -> [(ByteString, ByteString)] -> ByteString
stringToSign method end path = stringToSign' method end path . queryString

stringToSign' :: Method -> Endpoint -> Path -> QueryString -> ByteString
stringToSign' method end path params = BS.intercalate "\n"
    [ method
    , end
    , path
    , toString params
    ]

-- | Make signature from a parameter list.
signature :: Method -> Endpoint -> Path -> SecretKey -> SignatureMethod -> [(ByteString, ByteString)] -> ByteString
signature method end path secret sigm
    = signature' method end path secret sigm . queryString

-- | Make signature from a 'QueryString'.
signature' :: Method -> Endpoint -> Path -> SecretKey -> SignatureMethod -> QueryString -> ByteString
signature' method end path secret sigm params
    = HTTP.urlEncode True
    $ Base64.encode
    $ signedString sigm
  where
    strToSign = stringToSign' method end path params
    signedString HmacSHA256
        = mconcat
        $ BL.toChunks
        $ SHA.bytestringDigest
        $ SHA.hmacSha256 (toL secret) (toL strToSign)
    toL = BL.fromChunks . (:[])
