{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Char8
import System.Posix.Syslog
import Test.QuickCheck
import Test.QuickCheck.Property

instance Arbitrary Priority where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Facility where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary

main :: IO ()
main = do
  dontExplodeTest
  outputTest

{--
 This isn't a true test. Instead, we're passing the PERROR option (meaning
 syslog will also send messages to STDERR), sending a message that should be
 whitelisted by the priority mask, and sending a message that should be
 blacklisted by the priority mask. If hsyslog is working correctly, then only
 "hsyslog is working" should appear in your test log output.
--}
outputTest :: IO ()
outputTest = withSyslog config $ \syslog -> do
    syslog [Debug, Error] "%s%d hsyslog is working :)"
    syslog [Error] "hsyslog is not working :("
  where
    config = defaultConfig
        { options = [PERROR, NDELAY]
        , priorityMask = Mask [Debug, Alert]
        }

dontExplodeTest :: IO ()
dontExplodeTest = withSyslogTo defaultConfig $ \syslogTo -> do
    let
      prop_dontExplode :: [Facility] -> [Priority] -> ByteString -> Property
      prop_dontExplode facs pris msg = ioProperty $ do
          syslogTo facs pris msg
          return succeeded
    quickCheck prop_dontExplode
