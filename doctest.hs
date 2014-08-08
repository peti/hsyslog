-- doctest.hs

module Main ( main ) where

import Test.DocTest

main :: IO ()
main = doctest [ "dist/build/System/Posix/Syslog.hs" ]
