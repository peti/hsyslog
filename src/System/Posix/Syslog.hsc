{-# LANGUAGE
    CApiFFI
  , ForeignFunctionInterface
  , OverloadedStrings
  #-}

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

module System.Posix.Syslog
  ( -- * Marshaled Data Types
    Priority (..)
  , toPriority
  , fromPriority
  , Facility (..)
  , toFacility
  , fromFacility
  , Option (..)
  , toOption
  , fromOption
  , PriorityMask (..)
  , fromPriorityMask
    -- * Haskell API to syslog
  , SyslogConfig (..)
  , defaultConfig
  , withSyslog
  , SyslogFn
  , withSyslogTo
  , SyslogToFn
    -- * Low-level C functions
  , _openlog
  , _closelog
  , _setlogmask
  , _syslog
    -- ** Low-level C macros
    -- | See the
    -- <http://www.gnu.org/software/libc/manual/html_node/Submitting-Syslog-Messages.html GNU libc documentation>
    -- for their intended usage.
  , _LOG_MASK
  , _LOG_UPTO
  , _LOG_MAKEPRI
  ) where

import Control.Exception (bracket_)
import Data.Bits (Bits, (.|.))
import Data.ByteString (ByteString, useAsCString)
import Data.List (foldl')
import Foreign.C (CInt (..), CString (..))

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic)
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
  deriving ( Bounded, Enum, Eq, Show, Read
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
  deriving (Bounded, Enum, Eq, Show, Read)

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
  deriving (Bounded, Enum, Eq, Show)

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

-- |`withSyslog` options for the priority mask

data PriorityMask
  = NoMask          -- ^ allow all messages thru
  | Mask [Priority] -- ^ allow only messages with the priorities listed
  | UpTo Priority   -- ^ allow only messages down to and including the specified priority
  deriving (Eq, Show)

fromPriorityMask :: PriorityMask -> CInt
fromPriorityMask (Mask pris) = bitsOrWith (_LOG_MASK . fromPriority) pris
fromPriorityMask (UpTo pri) = _LOG_UPTO $ fromPriority pri
fromPriorityMask NoMask = 0

data SyslogConfig = SyslogConfig
  { identifier        :: ByteString   -- ^ string appended to each log message
  , options           :: [Option]     -- ^ options for syslog behavior
  , defaultFacilities :: [Facility]   -- ^ facilities logged to when none are provided
  , priorityMask      :: PriorityMask -- ^ filter by priority which messages are logged
  }
  deriving (Eq, Show)

-- |A practical default syslog config. You'll at least want to change the
-- identifier.

defaultConfig :: SyslogConfig
defaultConfig = SyslogConfig "hsyslog" [NDELAY] [USER] NoMask

-- |Bracket an 'IO' computation between calls to '_openlog', '_setlogmask', and
-- '_closelog', and provide a logging function which can be used as follows:
--
-- > main = withSyslog defaultConfig $ \syslog -> do
-- >          putStrLn "huhu"
-- >          syslog [Debug] "huhu"
--
-- Note that these are /process-wide/ settings, so multiple calls to
-- this function will interfere with each other in unpredictable ways.

withSyslog :: SyslogConfig -> (SyslogFn -> IO ()) -> IO ()
withSyslog config f =
    bracket_ (openSyslog config) closeSyslog $ do
      useAsCString escape (\e -> f $ syslog e [])
      return ()

-- |The type of logging function provided by 'withSyslog'.

type SyslogFn
  =  [Priority] -- ^ the priorities under which to log
  -> ByteString -- ^ the message to log
  -> IO ()

-- |Like 'withSyslog' but provides a function for logging to specific
-- facilities per message rather than the default facilities in your
-- 'SyslogConfig'.

withSyslogTo :: SyslogConfig -> (SyslogToFn -> IO ()) -> IO ()
withSyslogTo config f =
    bracket_ (openSyslog config) closeSyslog $ do
      useAsCString escape (f . syslog)
      return ()

-- |The type of function provided by 'withSyslogTo'.

type SyslogToFn
  =  [Facility] -- ^ the facilities to log to
  -> [Priority] -- ^ the priorities under which to log
  -> ByteString -- ^ the message to log
  -> IO ()

openSyslog :: SyslogConfig -> IO ()
openSyslog (SyslogConfig ident opts facs mask) = do
    useAsCString ident (\i -> _openlog i cOpts cFacs)
    _setlogmask cMask
    return ()
  where
    cFacs = bitsOrWith fromFacility facs
    cMask = fromPriorityMask mask
    cOpts = bitsOrWith fromOption opts

closeSyslog :: IO ()
closeSyslog = _closelog

syslog :: CString -> [Facility] -> [Priority] -> ByteString -> IO ()
syslog esc facs pris msg =
    useAsCString msg (_syslogUnescaped (makePri facs pris) esc)

escape :: ByteString
escape = "%s"

-- |Open a connection to the system logger for a program. The string
-- identifier passed as the first argument is prepended to every
-- message, and is typically set to the program name. The behavior is
-- unspecified by POSIX.1-2008 if that identifier is 'nullPtr'.

foreign import ccall unsafe "openlog" _openlog :: CString -> CInt -> CInt -> IO ()

-- |Close the descriptor being used to write to the system logger.

foreign import ccall unsafe "closelog" _closelog :: IO ()

-- |A process has a log priority mask that determines which calls to
-- syslog may be logged. All other calls will be ignored. Logging is
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

_syslog :: CInt -> CString -> IO ()
_syslog int str = useAsCString escape $ \e -> _syslogUnescaped int e str

foreign import capi "syslog.h LOG_MASK" _LOG_MASK :: CInt -> CInt
foreign import capi "syslog.h LOG_UPTO" _LOG_UPTO :: CInt -> CInt
foreign import capi "syslog.h LOG_MAKEPRI" _LOG_MAKEPRI :: CInt -> CInt -> CInt

-- internal functions

bitsOrWith :: (Bits b, Num b) => (a -> b) -> [a] -> b
bitsOrWith f = foldl' (\bits x -> f x .|. bits) 0

makePri :: [Facility] -> [Priority] -> CInt
makePri facs pris =
    _LOG_MAKEPRI (bitsOrWith fromFacility facs) (bitsOrWith fromPriority pris)

foreign import ccall unsafe "syslog" _syslogUnescaped
  :: CInt -> CString -> CString -> IO ()
