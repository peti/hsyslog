{-# OPTIONS -fffi #-}
{- |
   Module      :  Syslog
   Copyright   :  (c) 2004-10-14 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   FFI bindings to Unix's @syslog(3)@. Process this file
   with @hsc2hs@  to obtain a Haskell module.
-}

module Syslog where

import System.IO
import Control.Exception ( bracket_ )
import Foreign.C

#include <syslog.h>

-- * Marshaled Data Types

-- |Set the priority of a message logged via `syslog`.

data Priority
  = Emergency   -- ^ system is unusable
  | Alert       -- ^ action must be taken immediately
  | Critical    -- ^ critical conditions
  | Error       -- ^ error conditions
  | Warning     -- ^ warning conditions
  | Notice      -- ^ normal but significant condition
  | Info        -- ^ informational
  | Debug       -- ^ debug-level messages
  deriving (Eq, Bounded, Show)

instance Enum Priority where
  toEnum #{const LOG_EMERG}   = Emergency
  toEnum #{const LOG_ALERT}   = Alert
  toEnum #{const LOG_CRIT}    = Critical
  toEnum #{const LOG_ERR}     = Error
  toEnum #{const LOG_WARNING} = Warning
  toEnum #{const LOG_NOTICE}  = Notice
  toEnum #{const LOG_INFO}    = Info
  toEnum #{const LOG_DEBUG}   = Debug
  toEnum i = error ("Syslog.Priority cannot be mapped to value " ++ show i)

  fromEnum Emergency = #{const LOG_EMERG}
  fromEnum Alert     = #{const LOG_ALERT}
  fromEnum Critical  = #{const LOG_CRIT}
  fromEnum Error     = #{const LOG_ERR}
  fromEnum Warning   = #{const LOG_WARNING}
  fromEnum Notice    = #{const LOG_NOTICE}
  fromEnum Info      = #{const LOG_INFO}
  fromEnum Debug     = #{const LOG_DEBUG}

-- |Syslog knows different types of system facilities. This
-- information is usually used to channel mesages into
-- different log files, etc. If in doubt, 'USER' is the best
-- choice (and the system's default).

data Facility
  = KERN        -- ^ kernel messages
  | USER        -- ^ random user-level messages
  | MAIL        -- ^ mail system
  | DAEMON      -- ^ system daemons
  | AUTH        -- ^ security\/authorization messages
  | SYSLOG      -- ^ messages generated internally by syslogd
  | LPR         -- ^ line printer subsystem
  | NEWS        -- ^ network news subsystem
  | UUCP        -- ^ UUCP subsystem
  | CRON        -- ^ clock daemon
  | AUTHPRIV    -- ^ security\/authorization messages (private)
  | FTP         -- ^ ftp daemon
  | LOCAL0      -- ^ reserved for local use
  | LOCAL1      -- ^ reserved for local use
  | LOCAL2      -- ^ reserved for local use
  | LOCAL3      -- ^ reserved for local use
  | LOCAL4      -- ^ reserved for local use
  | LOCAL5      -- ^ reserved for local use
  | LOCAL6      -- ^ reserved for local use
  | LOCAL7      -- ^ reserved for local use
  deriving (Eq, Bounded, Show)

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
  | PERROR    -- ^ log to 'stderr' as well
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

-- |Bracket an 'IO' computation between calls to '_openlog'
-- and '_closelog'. Since these settings are for the
-- /process/, multiple calls to this function will,
-- unfortunately, overwrite each other.
--
-- Example:
--
-- > main = withSyslog "my-ident" [PID, PERROR] USER $ do
-- >          putStrLn "huhu"
-- >          syslog Debug "huhu"

withSyslog :: String -> [Option] -> Facility -> IO a -> IO a
withSyslog ident opts facil f = do
  let opt = toEnum . sum . map fromEnum $ opts
  let fac = toEnum . fromEnum           $ facil
  withCString ident $ \p ->
    bracket_ (_openlog p opt fac) (_closelog) f

-- |Log a message with the given priority.

syslog :: Priority -> String -> IO ()
syslog l msg =
  withCString (safeMsg msg)
    (\p -> _syslog (toEnum (fromEnum l)) p)

-- * Helpers

-- | @useSyslog ident@ @=@ @withSyslog ident [PID, PERROR] USER@

useSyslog :: String -> IO a -> IO a
useSyslog ident = withSyslog ident [PID, PERROR] USER

-- |Escape any occurances of \'@%@\' in a string, so that it
-- is safe to pass it to '_syslog'. The 'syslog' wrapper
-- does this automatically.

safeMsg :: String -> String
safeMsg []       = []
safeMsg ('%':xs) = '%' : '%' : safeMsg xs
safeMsg ( x :xs) = x : safeMsg xs

-- * Low-level C functions

foreign import ccall safe "closelog" _closelog ::
  IO ()

foreign import ccall safe "openlog" _openlog ::
  CString -> CInt -> CInt -> IO ()

foreign import ccall safe "setlogmask" _setlogmask ::
  CInt -> IO CInt

foreign import ccall safe "syslog" _syslog ::
  CInt -> CString -> IO ()
