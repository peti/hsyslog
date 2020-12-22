{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: POSIX

   A Haskell interface to @syslog(3)@ as specified in
   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html POSIX.1-2008>.
   The entire public API lives in this module. There is a set of exposed
   modules available underneath this one, which contain various implementation
   details that may be useful to other developers who want to implement
   syslog-related functionality. /Users/ of syslog, however, do not need those
   modules; "System.Posix.Syslog" has all you'll need.

   Check out the
   <https://github.com/peti/hsyslog/blob/master/example/Main.hs example program>
   that demonstrates how to use this library.
-}

module System.Posix.Syslog
  ( -- * Writing Log Messages
    syslog, Priority(..), Facility(..)
  , -- * Configuring the system's logging engine
    openlog, closelog, withSyslog, setlogmask, Option(..)
  )
  where

import System.Posix.Syslog.Facility
import System.Posix.Syslog.Functions
import System.Posix.Syslog.LogMask
import System.Posix.Syslog.Options
import System.Posix.Syslog.Priority

import Control.Exception ( assert, bracket_ )
import Data.Bits
import Foreign.C

-- | Log the message via the @syslog(3)@. Message expected to be formed and
-- committed to the log /verbatim/. @printf()@-style text formatting
-- features offered by the underlying system function are /not/ available
-- (for example, if log message reads @"%s"@, that string is exactly what will
-- be written to the log). C function terminates the string (message)
-- on the @\\0@ byte, so it dops all further message content, so messages
-- should not contain the @\\0@ byte.
--
-- The Haskell 'String' type can be easily logged with 'withCStringLen':
--
-- @
--  withCStringLen "Hello, world." $ syslog (Just User) Info
-- @
--
-- 'ByteStrings' can be logged in the same way with the 'unsafeUseAsCStringLen'
-- function from @Data.ByteString.Unsafe@, which extracts a 'CStringLen' from
-- the 'ByteString' in constant time (no copying!).
syslog ::
  Maybe Facility
  -- ^ Set the <https://tools.ietf.org/html/rfc5424#section-6.2.1> syslog facility
  -- for the message. If left unspecified, the process-wide default will be used, which
  -- tends to be 'User' by default.

       -> CStringLen     -- ^ The actual log message. The string does not need
                         -- to be terminated by a @\\0@ byte. If the string
                         -- /does/ contain a @\\0@ byte, then the message ends
                         -- there regardless of what the length argument says.
       -> IO ()
  -> Priority
  -- ^ Log with the specified priority Syslog severity:
  -- <https://tools.ietf.org/html/rfc5424#section-6.2.1>
  -- For logging, monitoring, alerting, troubleshooting purposes it is
  -- important to set severity of messages right, please read explanations in
  -- the RFC 5424, or follow short explanations at
  -- Priority level section of Arch Wiki:
  -- <https://wiki.archlinux.org/index.php/Systemd/Journal#Priority_level>.

syslog facil prio (ptr,len) = assert (len >= 0) $
  _syslog (maybe 0 fromFacility facil) (fromPriority prio) ptr (fromIntegral len)

-- | This function configures the process-wide hidden state of the system's
-- syslog engine. It's probably a bad idea to call this function anywhere
-- except at the very top of your program's 'main' function. And even then you
-- should probably prefer 'withSyslog' instead, which guarantees that syslog is
-- properly initialized within its scope.

openlog :: CString      -- ^ An identifier to prepend to all log messages,
                        -- typically the name of the program. Note that the
                        -- memory that contains this name must remain valid
                        -- until the pointer provided here is released by
                        -- calling 'closelog'.
        -> [Option]     -- ^ A set of options that configure the behavior of
                        -- the system's syslog engine.
        -> Facility     -- ^ The facility to use by default when none has been
                        -- specified with a 'syslog' call.
        -> IO ()
openlog ident opts facil =
  _openlog ident (foldr ((.|.) . fromOption) 0 opts) (fromFacility facil)

-- | Release all syslog-related resources.

closelog :: IO ()
closelog = _closelog

-- | Run the given @IO a@ computation within an initialized syslogging scope.
-- The definition is:
--
-- @
--   withSyslog ident opts facil f =
--     'withCString' ident $ \ptr ->
--       'bracket_' (openlog ptr opts facil) closelog f
-- @

withSyslog :: String -> [Option] -> Facility -> IO a -> IO a
withSyslog ident opts facil f =
  withCString ident $ \ptr ->
    bracket_ (openlog ptr opts facil) closelog f

-- | Configure a process-wide filter that determines which logging priorities
-- are ignored and which ones are forwarded to the @syslog@ implementation. For
-- example, use @setlogmask [Emergency .. Info]@ to filter out all debug-level
-- messages from the message stream. Calling @setlogmask [minBound..maxBound]@
-- enables /everything/. The special case @setlogmask []@ does /nothing/, i.e.
-- the current filter configuration is not modified. This can be used to
-- retrieve the current configuration.

setlogmask :: [Priority] -> IO [Priority]
setlogmask prios = fmap fromLogMask (_setlogmask (toLogMask prios))
