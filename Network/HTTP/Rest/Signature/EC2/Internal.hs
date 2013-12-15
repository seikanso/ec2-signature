module Network.HTTP.Rest.Signature.EC2.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Digest.Pure.SHA as SHA
import Data.List (sort)
import Data.Monoid
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types (Query, SimpleQuery)

type Method = ByteString
type Endpoint = ByteString
type Path = ByteString
type SecretKey = ByteString
data SignatureMethod = HmacSHA256

-- | Make a string for making signature.
--
-- >>> stringToSign "GET" "ec2.amazonaws.com" "/" [("key1", "value1"), ("key2", "value2")]
-- "GET\nec2.amazonaws.com\n/\nkey1=value1&key2=value2"
stringToSign :: Method -> Endpoint -> Path -> SimpleQuery -> ByteString
stringToSign method end path
    = stringToSign' method end path
    . HTTP.simpleQueryToQuery

stringToSign' :: Method -> Endpoint -> Path -> Query -> ByteString
stringToSign' method end path query = BS.intercalate "\n"
    [ method
    , end
    , path
    , HTTP.renderQuery False $ sort query
    ]

-- | Make signature from a parameter list.
signature :: Method -> Endpoint -> Path -> SecretKey -> SignatureMethod -> SimpleQuery -> ByteString
signature method end path secret sigm
    = signature' method end path secret sigm
    . HTTP.simpleQueryToQuery

-- | Make signature from a 'QueryString'.
signature' :: Method -> Endpoint -> Path -> SecretKey -> SignatureMethod -> Query -> ByteString
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
