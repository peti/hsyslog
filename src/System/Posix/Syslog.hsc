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
   <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/syslog.h.html POSIX.1-2008>.
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
    -- * Configuring syslog
  , SyslogConfig (..)
  , defaultConfig
    -- * The preferred Haskell API to syslog
  , withSyslog
  , SyslogFn
    -- * The unsafe Haskell API to syslog
  , syslogUnsafe
    -- * Low-level C functions
    -- | See the
    -- <http://pubs.opengroup.org/onlinepubs/9699919799/functions/closelog.html POSIX.1-2008 documentation>.
  , _openlog
  , _closelog
  , _setlogmask
  , _syslog
    -- ** Low-level C macros
  , _LOG_MAKEPRI
  , _LOG_MASK
  , _LOG_UPTO
    -- * Utilities
    -- | Low-level utilities for syslog-related tools
  , makePri
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

-- | Log messages have a priority attached.

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

-- | Syslog distinguishes various system facilities. Most applications should
-- log in 'USER'.

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
  deriving ( Bounded, Enum, Eq, Show, Read
#if __GLASGOW_HASKELL__ >= 706
           , Generic
#endif
           )

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

-- | 'withSyslog' options for the syslog service.

data Option
  = PID       -- ^ log the pid with each message
  | CONS      -- ^ log on the console if errors in sending
  | ODELAY    -- ^ delay open until first @syslog()@ (default)
  | NDELAY    -- ^ don't delay open
  | NOWAIT    -- ^ don't wait for console forks: DEPRECATED
  | PERROR    -- ^ log to 'stderr' as well (might be a no-op on some systems)
  deriving ( Bounded, Enum, Eq, Show, Read
#if __GLASGOW_HASKELL__ >= 706
           , Generic
#endif
           )

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

-- | 'withSyslog' options for the priority mask.

data PriorityMask
  = NoMask          -- ^ allow all messages thru
  | Mask [Priority] -- ^ allow only messages with the priorities listed
  | UpTo Priority   -- ^ allow only messages down to and including the specified priority
  deriving ( Eq, Show, Read
#if __GLASGOW_HASKELL__ >= 706
           , Generic
#endif
           )

fromPriorityMask :: PriorityMask -> CInt
fromPriorityMask (Mask pris) = bitsOrWith (_LOG_MASK . fromPriority) pris
fromPriorityMask (UpTo pri) = _LOG_UPTO $ fromPriority pri
fromPriorityMask NoMask = 0

data SyslogConfig = SyslogConfig
  { identifier :: ByteString
    -- ^ string appended to each log message
  , options :: [Option]
    -- ^ options for syslog behavior
  , defaultFacility :: Facility
    -- ^ facility logged to when none are provided (currently unsupported)
  , priorityMask :: PriorityMask
    -- ^ filter by priority which messages are logged
  }
  deriving (Eq, Show)

-- | A practical default syslog config. You'll at least want to change the
-- identifier.

defaultConfig :: SyslogConfig
defaultConfig = SyslogConfig "hsyslog" [ODELAY] USER NoMask

-- | Bracket an 'IO' computation between calls to '_openlog', '_setlogmask',
-- and '_closelog', providing a logging function which can be used as follows:
--
-- > main = withSyslog defaultConfig $ \syslog -> do
-- >          putStrLn "huhu"
-- >          syslog USER Debug "huhu"
--
-- Note that these are /process-wide/ settings, so multiple calls to
-- this function will interfere with each other in unpredictable ways.

withSyslog :: SyslogConfig -> (SyslogFn -> IO ()) -> IO ()
withSyslog config f =
    useAsCString (identifier config) $ \cIdent ->
      let
        open :: IO ()
        open = do
            _openlog cIdent cOpts cFac
            _setlogmask cMask
            return ()
          where
            cFac = fromFacility $ defaultFacility config
            cMask = fromPriorityMask $ priorityMask config
            cOpts = bitsOrWith fromOption $ options config

        close :: IO ()
        close = _closelog

        run :: IO ()
        run = do
            useAsCString escape (f . syslogEscaped)
            return ()
      in
        bracket_ open close run

-- | The type of function provided by 'withSyslog'.

type SyslogFn
  =  Facility -- ^ the facility to log to
  -> Priority -- ^ the priority under which to log
  -> ByteString -- ^ the message to log
  -> IO ()

-- | Provides no guarantee that a call to '_openlog' has been made, inviting
-- unpredictable results.

syslogUnsafe :: SyslogFn
syslogUnsafe fac pri msg = useAsCString msg (_syslog (makePri fac pri))

-- foreign imports

foreign import ccall unsafe "openlog" _openlog :: CString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "closelog" _closelog :: IO ()
foreign import ccall unsafe "setlogmask" _setlogmask :: CInt -> IO CInt

foreign import ccall unsafe "syslog" _syslogEscaped
  :: CInt -> CString -> CString -> IO ()

_syslog :: CInt -> CString -> IO ()
_syslog int msg = useAsCString escape $ \e -> _syslogEscaped int e msg

foreign import capi "syslog.h LOG_MAKEPRI" _LOG_MAKEPRI :: CInt -> CInt -> CInt
foreign import capi "syslog.h LOG_MASK" _LOG_MASK :: CInt -> CInt
foreign import capi "syslog.h LOG_UPTO" _LOG_UPTO :: CInt -> CInt

-- utilities

-- | Calculate the full priority value of a 'Facility' and 'Priority'

makePri :: Facility -> Priority -> CInt
makePri fac pri = _LOG_MAKEPRI (fromFacility fac) (fromPriority pri)

-- internal functions

bitsOrWith :: (Bits b, Num b) => (a -> b) -> [a] -> b
bitsOrWith f = foldl' (\bits x -> f x .|. bits) 0

escape :: ByteString
escape = "%s"

syslogEscaped :: CString -> Facility -> Priority -> ByteString -> IO ()
syslogEscaped esc fac pri msg =
    useAsCString msg (_syslogEscaped (makePri fac pri) esc)
