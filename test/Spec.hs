{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Network.HTTP.Types (SimpleQuery)
import Network.HTTP.Rest.Signature.EC2

instance Arbitrary ByteString where
    arbitrary = BC.pack <$> arbitrary

data KeyString = Key { toBS :: ByteString }

instance Arbitrary KeyString where
    arbitrary = Key . BC.pack <$> ((:) <$> arbitrary <*> arbitrary)

data T = T SimpleQuery
  deriving (Show)

instance Arbitrary T where
    arbitrary = T . map (\(a, b) -> (toBS a, b)) <$> arbitrary

main :: IO ()
main = hspec $ do
    describe "Network.HTTP.Rest.Signature.EC2" $ do
        it "calculate signature" testSignature
        prop "calculate signature" propSignature

testSignature :: IO ()
testSignature = do
    signature
            "GET"
            "ec2.amazonaws.com"
            "/"
            "secretKey"
            HmacSHA256
            [("key1", "value1"), ("key2", "value2")]
        @=? "KkzU%2BMu0a1x8qa3uHhXBGWVVTcKV3dTLcgkCIIeAyZw%3D"
    signature
            "GET"
            "ec2.amazonaws.com"
            "/"
            "secretKey2"
            HmacSHA256
            [("key1", "value1"), ("key2", "value2")]
        @=? "TzOwg4QGXgL%2F9KFOLbddgqMH4zDIqUv9fdBMU%2F8X8Hg%3D"

propSignature :: T -> Bool
propSignature (T query) = f query == f (reverse query)
  where
    f = signature
            "GET"
            "ec2.amazonaws.com"
            "/"
            "secretKey"
            HmacSHA256
