{-# LANGUAGE ForeignFunctionInterface #-}

{- |
   Maintainer: simons@cryp.to
   Stability: provisional
   Portability: POSIX

   Low-level FFI bindings to @syslog(3)@ et al from
   <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/syslog.h.html POSIX.1-2008>.
   This module is intended for purposes of low-level implementation. Users of
   this library should prefer safer and more convenient API provided by
   "System.Posix.Syslog"
-}

module System.Posix.Syslog.Functions where

import Foreign.C

-- | The POSIX function <http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html syslog(3)>
-- imported into Haskell directly as an "unsafe" C-API call. We chose this
-- specific signature for the variadic function, because it's ideal for the
-- efficient zero-copy implementation provided by the high-level function
-- 'System.Posix.Syslog.syslog'.
foreign import ccall unsafe "syslog.h simpleSyslog" _syslog
  :: CInt       -- ^ The system-specific identifier for 'System.Posix.Syslog.Facility'.
  -> CInt       -- ^ The system-specific identifier for 'System.Posix.Syslog.Priority'.
  -> CString    -- ^ The actual log message, which does not need to be
                -- terminated by a NUL byte. It should not contain NUL bytes
                -- either, though.
  -> CInt       -- ^ The length of the log message. Yes, this is a signed
                -- integer. Yes, an unsigned integer would be better. No, I
                -- can't do anything about it. It's frickin' C code from one
                -- and a half centuries ago; what do you expect? Just don't
                -- pass any negative values here, okay?
  -> IO ()

-- | The POSIX function <http://pubs.opengroup.org/onlinepubs/9699919799/functions/openlog.html openlog(3)>,
-- imported into Haskell directly as an "unsafe" foreign function call.
foreign import ccall unsafe "syslog.h openlog" _openlog
  :: CString    -- ^ A process-wide identifier to prepent to every log message.
                -- Note that this string must exist until 'closelog' is called.
                -- If the underlying memory buffer changes, the identifier used
                -- by 'System.Posix.Syslog.syslog' /probably/ changes too. It's
                -- safe to pass 'nullPtr', but POSIX does not specify how that
                -- choice is
                -- interpreted.
  -> CInt       -- ^ A bit set that combines various 'Option' values.
  -> CInt       -- ^ A default 'Facility' to use for messages that don't
                -- specify one.
  -> IO ()

-- | The POSIX function <http://pubs.opengroup.org/onlinepubs/9699919799/functions/closelog.html closelog(3)>
-- imported into Haskell directly as an "unsafe" foreign function call.
foreign import ccall unsafe "syslog.h closelog" _closelog :: IO ()

-- | The POSIX function <http://pubs.opengroup.org/onlinepubs/9699919799/functions/setlogmask.html setlogmask(3)>
-- imported into Haskell directly as an "unsafe" foreign function call.
foreign import ccall unsafe "syslog.h setlogmask" _setlogmask
  :: CInt       -- ^ A bit mask that determines which priorities are enabled or
                -- disabled. See also '_LOG_MASK'.
  -> IO CInt

-- | The POSIX macro <http://pubs.opengroup.org/onlinepubs/009695399/basedefs/syslog.h.html LOG_MASK()>
-- imported into Haskell directly as a pure, "unsafe" foreign function call. It
-- does feel a little silly to bother with this functions since we pretty much know
-- @
--   _logMask = (2^)
-- @
-- for certain, but, well, POSIX provides this abstraction and so it's probably
-- no good idea to make that assumption.
foreign import ccall unsafe "makeLogMask" _logMask :: CInt -> CInt
