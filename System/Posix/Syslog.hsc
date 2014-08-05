{-# LANGUAGE ForeignFunctionInterface #-}
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

-- |Log messages are prioritized.

data Priority
  = Emergency   -- ^ system is unusable
  | Alert       -- ^ action must be taken immediately
  | Critical    -- ^ critical conditions
  | Error       -- ^ error conditions
  | Warning     -- ^ warning conditions
  | Notice      -- ^ normal but significant condition
  | Info        -- ^ informational
  | Debug       -- ^ debug-level messages
  deriving ( Eq, Bounded, Show, Read
#if __GLASGOW_HASKELL__ >= 706
           , Generic
#endif
           )

instance Enum Priority where
  toEnum #{const LOG_EMERG}   = Emergency
  toEnum #{const LOG_ALERT}   = Alert
  toEnum #{const LOG_CRIT}    = Critical
  toEnum #{const LOG_ERR}     = Error
  toEnum #{const LOG_WARNING} = Warning
  toEnum #{const LOG_NOTICE}  = Notice
  toEnum #{const LOG_INFO}    = Info
  toEnum #{const LOG_DEBUG}   = Debug
  toEnum i = error (showString "Syslog.Priority cannot be mapped from value " (show i))

  fromEnum Emergency = #{const LOG_EMERG}
  fromEnum Alert     = #{const LOG_ALERT}
  fromEnum Critical  = #{const LOG_CRIT}
  fromEnum Error     = #{const LOG_ERR}
  fromEnum Warning   = #{const LOG_WARNING}
  fromEnum Notice    = #{const LOG_NOTICE}
  fromEnum Info      = #{const LOG_INFO}
  fromEnum Debug     = #{const LOG_DEBUG}

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
  deriving (Eq, Bounded, Show, Read)

instance Enum Facility where
  toEnum #{const LOG_KERN}      = KERN
  toEnum #{const LOG_USER}      = USER
  toEnum #{const LOG_MAIL}      = MAIL
  toEnum #{const LOG_DAEMON}    = DAEMON
  toEnum #{const LOG_AUTH}      = AUTH
  toEnum #{const LOG_SYSLOG}    = SYSLOG
  toEnum #{const LOG_LPR}       = LPR
  toEnum #{const LOG_NEWS}      = NEWS
  toEnum #{const LOG_UUCP}      = UUCP
  toEnum #{const LOG_CRON}      = CRON
  toEnum #{const LOG_AUTHPRIV}  = AUTHPRIV
  toEnum #{const LOG_FTP}       = FTP
  toEnum #{const LOG_LOCAL0}    = LOCAL0
  toEnum #{const LOG_LOCAL1}    = LOCAL1
  toEnum #{const LOG_LOCAL2}    = LOCAL2
  toEnum #{const LOG_LOCAL3}    = LOCAL3
  toEnum #{const LOG_LOCAL4}    = LOCAL4
  toEnum #{const LOG_LOCAL5}    = LOCAL5
  toEnum #{const LOG_LOCAL6}    = LOCAL6
  toEnum #{const LOG_LOCAL7}    = LOCAL7
  toEnum i = error ("Syslog.Facility cannot be mapped to value " ++ show i)

  fromEnum KERN      = #{const LOG_KERN}
  fromEnum USER      = #{const LOG_USER}
  fromEnum MAIL      = #{const LOG_MAIL}
  fromEnum DAEMON    = #{const LOG_DAEMON}
  fromEnum AUTH      = #{const LOG_AUTH}
  fromEnum SYSLOG    = #{const LOG_SYSLOG}
  fromEnum LPR       = #{const LOG_LPR}
  fromEnum NEWS      = #{const LOG_NEWS}
  fromEnum UUCP      = #{const LOG_UUCP}
  fromEnum CRON      = #{const LOG_CRON}
  fromEnum AUTHPRIV  = #{const LOG_AUTHPRIV}
  fromEnum FTP       = #{const LOG_FTP}
  fromEnum LOCAL0    = #{const LOG_LOCAL0}
  fromEnum LOCAL1    = #{const LOG_LOCAL1}
  fromEnum LOCAL2    = #{const LOG_LOCAL2}
  fromEnum LOCAL3    = #{const LOG_LOCAL3}
  fromEnum LOCAL4    = #{const LOG_LOCAL4}
  fromEnum LOCAL5    = #{const LOG_LOCAL5}
  fromEnum LOCAL6    = #{const LOG_LOCAL6}
  fromEnum LOCAL7    = #{const LOG_LOCAL7}

-- |Options for the syslog service. Set with 'withSyslog'.

data Option
  = PID       -- ^ log the pid with each message
  | CONS      -- ^ log on the console if errors in sending
  | ODELAY    -- ^ delay open until first @syslog()@ (default)
  | NDELAY    -- ^ don't delay open
  | NOWAIT    -- ^ don't wait for console forks: DEPRECATED
  | PERROR    -- ^ log to 'stderr' as well (might be a no-op on some systems)
  deriving (Eq, Bounded, Show)

instance Enum Option where
  toEnum #{const LOG_PID}     = PID
  toEnum #{const LOG_CONS}    = CONS
  toEnum #{const LOG_ODELAY}  = ODELAY
  toEnum #{const LOG_NDELAY}  = NDELAY
  toEnum #{const LOG_NOWAIT}  = NOWAIT
  toEnum #{const LOG_PERROR}  = PERROR
  toEnum i = error ("Syslog.Option cannot be mapped to value " ++ show i)

  fromEnum PID     = #{const LOG_PID}
  fromEnum CONS    = #{const LOG_CONS}
  fromEnum ODELAY  = #{const LOG_ODELAY}
  fromEnum NDELAY  = #{const LOG_NDELAY}
  fromEnum NOWAIT  = #{const LOG_NOWAIT}
  fromEnum PERROR  = #{const LOG_PERROR}

-- * Haskell API to syslog

-- |Bracket an 'IO' computation between calls to '_openlog',
-- '_setlogmask', and '_closelog'. Since these settings are for the
-- /process/, multiple calls to this function will, unfortunately,
-- overwrite each other.
--
-- Example:
--
-- > main = withSyslog "my-ident" [PID, PERROR] USER (logUpTo Debug) $ do
-- >          putStrLn "huhu"
-- >          syslog Debug "huhu"

withSyslog :: String -> [Option] -> Facility -> [Priority] -> IO a -> IO a
withSyslog ident opts facil prio f = withCString ident $ \p ->
    bracket_ (_openlog p opt fac >> _setlogmask pri) (_closelog) f
  where
    fac = toEnum . fromEnum           $ facil
    pri = toEnum . foldl1 (.|.) . map (shift 1 . fromEnum) $ if null prio
                                                             then [minBound .. maxBound]
                                                             else prio
    opt = toEnum . sum . map fromEnum $ opts

-- |Log a message with the given priority.

syslog :: Priority -> String -> IO ()
syslog l msg =
  withCString (safeMsg msg)
    (\p -> _syslog (toEnum (fromEnum l)) p)

-- * Helpers

-- | @useSyslog ident@ @=@ @withSyslog ident [PID, PERROR] USER (logUpTo Debug)@

useSyslog :: String -> IO a -> IO a
useSyslog ident = withSyslog ident [PID, PERROR] USER (logUpTo Debug)

-- |Returns the list of priorities up to and including the argument.
logUpTo :: Priority -> [Priority]
logUpTo p = [minBound .. p]

-- |Escape any occurances of \'@%@\' in a string, so that it
-- is safe to pass it to '_syslog'. The 'syslog' wrapper
-- does this automatically.

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
