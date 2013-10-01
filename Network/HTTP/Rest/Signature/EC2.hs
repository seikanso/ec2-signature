module Network.HTTP.Rest.Signature.EC2
    ( -- * Re-exports
      module Network.HTTP.QueryString
      -- * Types
    , Method
    , Endpoint
    , Path
    , SecretKey
    , SignatureMethod(..)
      -- * Building signature
    , stringToSign
    , stringToSign'
    , signature
    , signature'
    ) where

import Network.HTTP.QueryString
import Network.HTTP.Rest.Signature.EC2.Internal
