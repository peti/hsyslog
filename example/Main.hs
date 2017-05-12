-- These extensions are required so that we can define a class instance for "String".
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main ( main ) where

import System.Posix.Syslog

import Data.ByteString.Char8 ( ByteString, pack )
import Data.ByteString.Unsafe ( unsafeUseAsCStringLen )
import Foreign.C.String ( CStringLen, withCStringLen )

-- This class allows us to log normal Strings, ByteStrings, and pretty much any
-- other type to syslog without. It abstracts the information of how to convert
-- the given type into a CStringLen that can be passed to syslog.

class LogMessage m where
  toCStringLen :: m -> (CStringLen -> IO a) -> IO a

instance LogMessage String where
  toCStringLen = withCStringLen

instance LogMessage ByteString where
  toCStringLen = unsafeUseAsCStringLen

-- This simplified syslog interface can deal efficiently with any LogMessage.
-- It relies on the default 'Facility' to be configured globally.

write :: LogMessage a => Priority -> a -> IO ()
write pri msg = toCStringLen msg (syslog Nothing pri)

-- Now write a couple of String and ByteString messages. On my system, the log
-- file shows the following output:
--
--    May 12 19:49:18 myhost example[26995]: Hello, World.
--    May 12 19:49:18 myhost example[26995]: Default logging mask is [Emergency,Alert,Critical,Error,Warning,Notice,Info,Debug]

main :: IO ()
main =
  withSyslog "example" [LogPID, Console] User $ do
    write Info "Hello, World."
    lm <- setlogmask [Debug]
    write Info (pack "This message does not show up.")
    write Debug ("Default logging mask is " ++ show lm)
