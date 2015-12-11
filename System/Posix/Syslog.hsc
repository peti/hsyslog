{-# LANGUAGE CApiFFI, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric #-}
#endif
{- |
   Module      :  System.Posix.Syslog
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Posix

   FFI bindings to syslog(3) from
   <http://www.opengroup.org/onlinepubs/009695399/basedefs/syslog.h.html POSIX.1-2001>.
-}

module System.Posix.Syslog where

import Control.Exception ( bracket_ )
import Data.Bits
import Data.List (foldl')
import Foreign.C
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics
#endif

#include <syslog.h>
#ifndef LOG_AUTHPRIV
#define LOG_AUTHPRIV LOG_AUTH
#endif

#ifndef LOG_FTP
#define LOG_FTP LOG_DAEMON
#endif

#ifndef LOG_PERROR
#define LOG_PERROR 0
#endif

-- * Marshaled Data Types

-- |Log messages have a priority attached.

data Priority
  = Emergency   -- ^ system is unusable
  | Alert       -- ^ action must be taken immediately
  | Critical    -- ^ critical conditions
  | Error       -- ^ error conditions
  | Warning     -- ^ warning conditions
  | Notice      -- ^ normal but significant condition
  | Info        -- ^ informational
  | Debug       -- ^ debug-level messages
  deriving ( Eq, Show, Read
#if __GLASGOW_HASKELL__ >= 706
           , Generic
#endif
           )

toPriority :: CInt -> Priority
toPriority #{const LOG_EMERG}   = Emergency
toPriority #{const LOG_ALERT}   = Alert
toPriority #{const LOG_CRIT}    = Critical
toPriority #{const LOG_ERR}     = Error
toPriority #{const LOG_WARNING} = Warning
toPriority #{const LOG_NOTICE}  = Notice
toPriority #{const LOG_INFO}    = Info
toPriority #{const LOG_DEBUG}   = Debug
toPriority i = error (shows i " is not a valid syslog priority value")

fromPriority :: Priority -> CInt
fromPriority Emergency = #{const LOG_EMERG}
fromPriority Alert     = #{const LOG_ALERT}
fromPriority Critical  = #{const LOG_CRIT}
fromPriority Error     = #{const LOG_ERR}
fromPriority Warning   = #{const LOG_WARNING}
fromPriority Notice    = #{const LOG_NOTICE}
fromPriority Info      = #{const LOG_INFO}
fromPriority Debug     = #{const LOG_DEBUG}

-- |Syslog distinguishes various system facilities. Most
-- applications should log in 'USER'.

data Facility
  = KERN        -- ^ kernel messages
  | USER        -- ^ user-level messages (default unless set otherwise)
  | MAIL        -- ^ mail system
  | DAEMON      -- ^ system daemons
  | AUTH        -- ^ security\/authorization messages
  | SYSLOG      -- ^ messages generated internally by syslogd
  | LPR         -- ^ line printer subsystem
  | NEWS        -- ^ network news subsystem
  | UUCP        -- ^ UUCP subsystem
  | CRON        -- ^ clock daemon
  | AUTHPRIV    -- ^ security\/authorization messages (effectively equals 'AUTH' on some systems)
  | FTP         -- ^ ftp daemon (effectively equals 'DAEMON' on some systems)
  | LOCAL0      -- ^ reserved for local use
  | LOCAL1      -- ^ reserved for local use
  | LOCAL2      -- ^ reserved for local use
  | LOCAL3      -- ^ reserved for local use
  | LOCAL4      -- ^ reserved for local use
  | LOCAL5      -- ^ reserved for local use
  | LOCAL6      -- ^ reserved for local use
  | LOCAL7      -- ^ reserved for local use
  deriving (Eq, Show, Read)

toFacility :: CInt -> Facility
toFacility #{const LOG_KERN}      = KERN
toFacility #{const LOG_USER}      = USER
toFacility #{const LOG_MAIL}      = MAIL
toFacility #{const LOG_DAEMON}    = DAEMON
toFacility #{const LOG_AUTH}      = AUTH
toFacility #{const LOG_SYSLOG}    = SYSLOG
toFacility #{const LOG_LPR}       = LPR
toFacility #{const LOG_NEWS}      = NEWS
toFacility #{const LOG_UUCP}      = UUCP
toFacility #{const LOG_CRON}      = CRON
toFacility #{const LOG_AUTHPRIV}  = AUTHPRIV
toFacility #{const LOG_FTP}       = FTP
toFacility #{const LOG_LOCAL0}    = LOCAL0
toFacility #{const LOG_LOCAL1}    = LOCAL1
toFacility #{const LOG_LOCAL2}    = LOCAL2
toFacility #{const LOG_LOCAL3}    = LOCAL3
toFacility #{const LOG_LOCAL4}    = LOCAL4
toFacility #{const LOG_LOCAL5}    = LOCAL5
toFacility #{const LOG_LOCAL6}    = LOCAL6
toFacility #{const LOG_LOCAL7}    = LOCAL7
toFacility i = error (shows i " is not a valid syslog facility value")

fromFacility :: Facility -> CInt
fromFacility KERN      = #{const LOG_KERN}
fromFacility USER      = #{const LOG_USER}
fromFacility MAIL      = #{const LOG_MAIL}
fromFacility DAEMON    = #{const LOG_DAEMON}
fromFacility AUTH      = #{const LOG_AUTH}
fromFacility SYSLOG    = #{const LOG_SYSLOG}
fromFacility LPR       = #{const LOG_LPR}
fromFacility NEWS      = #{const LOG_NEWS}
fromFacility UUCP      = #{const LOG_UUCP}
fromFacility CRON      = #{const LOG_CRON}
fromFacility AUTHPRIV  = #{const LOG_AUTHPRIV}
fromFacility FTP       = #{const LOG_FTP}
fromFacility LOCAL0    = #{const LOG_LOCAL0}
fromFacility LOCAL1    = #{const LOG_LOCAL1}
fromFacility LOCAL2    = #{const LOG_LOCAL2}
fromFacility LOCAL3    = #{const LOG_LOCAL3}
fromFacility LOCAL4    = #{const LOG_LOCAL4}
fromFacility LOCAL5    = #{const LOG_LOCAL5}
fromFacility LOCAL6    = #{const LOG_LOCAL6}
fromFacility LOCAL7    = #{const LOG_LOCAL7}

-- |'withSyslog' options for the syslog service.

data Option
  = PID       -- ^ log the pid with each message
  | CONS      -- ^ log on the console if errors in sending
  | ODELAY    -- ^ delay open until first @syslog()@ (default)
  | NDELAY    -- ^ don't delay open
  | NOWAIT    -- ^ don't wait for console forks: DEPRECATED
  | PERROR    -- ^ log to 'stderr' as well (might be a no-op on some systems)
  deriving (Eq, Show)

toOption :: CInt -> Option
toOption #{const LOG_PID}     = PID
toOption #{const LOG_CONS}    = CONS
toOption #{const LOG_ODELAY}  = ODELAY
toOption #{const LOG_NDELAY}  = NDELAY
toOption #{const LOG_NOWAIT}  = NOWAIT
toOption #{const LOG_PERROR}  = PERROR
toOption i = error (shows i " is not a valid syslog option value")

fromOption :: Option -> CInt
fromOption PID     = #{const LOG_PID}
fromOption CONS    = #{const LOG_CONS}
fromOption ODELAY  = #{const LOG_ODELAY}
fromOption NDELAY  = #{const LOG_NDELAY}
fromOption NOWAIT  = #{const LOG_NOWAIT}
fromOption PERROR  = #{const LOG_PERROR}

-- |Syslog provides two possibilities for `_setlogmask`: either a manual
-- whitelist of allowed priorities or an inclusive whitelist denoted by the
-- lowest allowed priority (opposite of what the naming seemingly dictates)

data PriorityMask
  = Mask [Priority]
  | UpTo Priority
  deriving (Eq, Show)

fromPriorityMask :: PriorityMask -> CInt
fromPriorityMask (Mask pris) = bitsOrWith (_LOG_MASK . fromPriority) pris
fromPriorityMask (UpTo pri) = _LOG_UPTO $ fromPriority pri

-- * Haskell API to syslog

-- |Bracket an 'IO' computation between calls to '_openlog',
-- '_setlogmask', and '_closelog'. The function can be used as follows:
--
-- > main = withSyslog "my-ident" [PID, PERROR] USER (UpTo Debug) $ do
-- >          putStrLn "huhu"
-- >          syslog Debug "huhu"
--
-- Note that these are /process-wide/ settings, so multiple calls to
-- this function will interfere with each other in unpredictable ways.

withSyslog :: String -> [Option] -> Facility -> PriorityMask -> IO a -> IO a
withSyslog ident opts facil mask f = withCString ident $ \p ->
    bracket_ (_openlog p optsInt facInt >> _setlogmask priInt) (_closelog) f
  where
    facInt = fromFacility facil
    priInt = fromPriorityMask mask
    optsInt = bitsOrWith fromOption opts

-- |Log a message with the given priority.
--
-- Note that the API of this function is somewhat unsatisfactory and is
-- likely to change in the future:
--
-- 1. The function should accept a @['Facility']@ argument so that
--    messages can be logged to certain facilities without depending on
--    the process-wide global default value set by 'openlog'
--    (<https://github.com/peti/hsyslog/issues/6 issue #6>).
--
-- 2. The 'Priority' argument should be @['Priority']@.
--
-- 3. Accepting a 'ByteString' instead of 'String' would be preferrable
--    because we can log those more efficiently, i.e. without
--    marshaling. On top of that, we can provide a wrapper for this
--    function that accepts anything that can be marshaled into a
--    'ByteString' (<https://github.com/peti/hsyslog/issues/7 issue #7>).

syslog :: Priority -> String -> IO ()
syslog pri msg = withCString (safeMsg msg) (_syslog (fromPriority pri))

-- * Helpers

-- |Escape any occurances of \'@%@\' in a string, so that it is safe to
-- pass it to '_syslog'. The 'syslog' wrapper does this automatically.
--
-- Unfortunately, the application of this function to every single
-- syslog message is a performence nightmare. Instead, we should call
-- syslog the existence of this function is a kludge, in a way that
-- doesn't require any escaping
-- (<https://github.com/peti/hsyslog/issues/8 issue #8>).

safeMsg :: String -> String
safeMsg []       = []
safeMsg ('%':xs) = '%' : '%' : safeMsg xs
safeMsg ( x :xs) = x : safeMsg xs

-- * Low-level C functions

-- |Open a connection to the system logger for a program. The string
-- identifier passed as the first argument is prepended to every
-- message, and is typically set to the program name. The behavior is
-- unspecified by POSIX.1-2008 if that identifier is 'nullPtr'.

foreign import ccall unsafe "openlog" _openlog :: CString -> CInt -> CInt -> IO ()

-- |Close the descriptor being used to write to the system logger.

foreign import ccall unsafe "closelog" _closelog :: IO ()

-- |A process has a log priority mask that determines which calls to
-- 'syslog' may be logged. All other calls will be ignored. Logging is
-- enabled for the priorities that have the corresponding bit set in
-- mask. The initial mask is such that logging is enabled for all
-- priorities. This function sets this logmask for the calling process,
-- and returns the previous mask. If the mask argument is 0, the current
-- logmask is not modified.

foreign import ccall unsafe "setlogmask" _setlogmask :: CInt -> IO CInt

-- |Generate a log message, which will be distributed by @syslogd(8)@.
-- The priority argument is formed by ORing the facility and the level
-- values (explained below). The remaining arguments are a format, as in
-- printf(3) and any arguments required by the format, except that the
-- two character sequence %m will be replaced by the error message
-- string strerror(errno). A trailing newline may be added if needed.

foreign import ccall unsafe "syslog" _syslog :: CInt -> CString -> IO ()

-- macros provided by syslog.h

foreign import capi "syslog.h LOG_MASK" _LOG_MASK :: CInt -> CInt
foreign import capi "syslog.h LOG_UPTO" _LOG_UPTO :: CInt -> CInt

-- utility functions

bitsOrWith :: (Bits b, Num b) => (a -> b) -> [a] -> b
bitsOrWith f = foldl' (\bits x -> f x .|. bits) 0
