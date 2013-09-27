{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Network.HTTP.Rest.Signature.EC2.Internal

instance Arbitrary ByteString where
    arbitrary = BC.pack <$> arbitrary

data KeyString = Key { unpack :: ByteString }

instance Arbitrary KeyString where
    arbitrary = Key . BC.pack <$> ((:) <$> arbitrary <*> arbitrary)

instance Arbitrary QueryString where
    arbitrary = queryString . map f <$> arbitrary
      where
        f (a, b) = (unpack a, b)

instance Show QueryString where
    show = show . rawData

main :: IO ()
main = hspec $ do
    describe "QueryString" $ do
        prop "mappend" prop_mappend

prop_mappend :: QueryString -> QueryString -> Bool
prop_mappend a b = (a <> b) == queryString (rawData a ++ rawData b)
