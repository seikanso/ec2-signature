module Network.HTTP.Rest.Signature.EC2
    ( 
      -- * Types
      Method
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

import Network.HTTP.Rest.Signature.EC2.Internal
