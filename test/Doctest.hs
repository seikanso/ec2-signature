module Main where

import Test.DocTest

main :: IO ()
main = doctest
    [ "-XOverloadedStrings"
    , "Network/HTTP/Rest/Signature/EC2.hs"
    ]
