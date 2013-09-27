module Network.HTTP.Rest.Signature.EC2
    ( -- * Types
      QueryString
    , Method
    , Endpoint
    , Path
    , SecretKey
    , SignatureMethod(..)
      -- * Building signature
    , toString
    , queryString
    , queryStringFromMap
    , stringToSign
    , stringToSign'
    , signature
    , signature'
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Digest.Pure.SHA as SHA
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Network.HTTP.Types as HTTP

-- | A query string for HTTP.
--
-- > "param1=value1&param2=value2"
data QueryString = QueryString { toString :: ByteString }
  deriving (Eq)

instance Monoid QueryString where
    mempty = QueryString ""
    mappend a b = QueryString $ toString a <> "&" <> toString b

type Method = ByteString
type Endpoint = ByteString
type Path = ByteString
type SecretKey = ByteString
data SignatureMethod = HmacSHA256

-- | Convert a parameter list to 'QueryString'.
--
-- >>> toString $ queryString [("param1", "value1"), ("param2", "value2")]
-- "param1=value1&param2=value2"
queryString :: [(ByteString, ByteString)] -> QueryString
queryString = queryString' . sort

-- | Convert a parameter map to 'QueryString'.
queryStringFromMap :: Map ByteString ByteString -> QueryString
queryStringFromMap = queryString' . Map.toList

queryString' :: [(ByteString, ByteString)] -> QueryString
queryString' = QueryString . BS.intercalate "&" . map concatWithEqual
  where
    concatWithEqual (key, val) = mconcat
        [ key
        , "="
        , HTTP.urlEncode True val
        ]

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
