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
    , parseQuery
    , queryString
    , queryStringFromMap
    , stringToSign
    , stringToSign'
    , signature
    , signature'
    ) where

import Network.HTTP.Rest.Signature.EC2.Internal
