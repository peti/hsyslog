{-# LANGUAGE DeriveGeneric #-}

{- |
   Maintainer: simons@cryp.to
   Stability: provisional
   Portability: POSIX

   FFI bindings to @syslog(3)@ from
   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html POSIX.1-2008>.
   This module is intended for purposes of low-level implementation. Users of
   this library should prefer safer and more convenient API provided by
   "System.Posix.Syslog".
-}

module System.Posix.Syslog.Priority where

import Foreign.C
import GHC.Generics ( Generic )

#include <syslog.h>

-- * Message Priorities

-- | Log messages are prioritized with one of the following levels:
--
-- >>> [minBound..maxBound] :: [Priority]
-- [Emergency,Alert,Critical,Error,Warning,Notice,Info,Debug]
--
-- The 'Ord' instance for 'Priority' considers the more urgent level lower than
-- less urgent ones:
--
-- >>> Emergency < Debug
-- True
-- >>> minimum [minBound..maxBound] :: Priority
-- Emergency
-- >>> maximum [minBound..maxBound] :: Priority
-- Debug

data Priority

  = Emergency
  -- ^ Operational System is unusable.
  -- This level should not be used by applications.

  | Alert
  -- ^ Something in OS should be attended to & corrected immediately:
  -- vital OS subsystem fails (networking, filesystem), OS state resulted in
  -- unrecoverable state and data loss. Reserved for OS critical parts.

  | Critical
  -- ^ Critical conditions: unexpected errors, error throws that result in
  -- application crashes, coredumps.
  -- Example:
  -- A crash of a data base (with possible data loss): <https://web.archive.org/web/20200511072730/https://www.postgresql.org/docs/12/runtime-config-logging.html#RUNTIME-CONFIG-SEVERITY-LEVELS> is this level of severity.

  | Error
  -- ^ Error conditions: error that is handled, application closes
  -- and returns the error code.

  | Warning
  -- ^ Action of solution is expected/required from the user,
  -- but so far application can/would function further,
  -- error may occur somewhere further if the action is not taken.

  | Notice
  -- ^ Events/states that are unusual, special case happened
  -- and handled properly, unusual cases that are not error conditions.

  | Info
  -- ^ Default level. Normal operational messages that require no action.

  | Debug
  -- ^ Information useful for debugging the application processes.
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Translate a 'Priority' into the system-dependent identifier that's used by
-- the @syslog(3)@ implementation.

{-# INLINE fromPriority #-}
fromPriority :: Priority -> CInt
fromPriority Emergency = #{const LOG_EMERG}
fromPriority Alert     = #{const LOG_ALERT}
fromPriority Critical  = #{const LOG_CRIT}
fromPriority Error     = #{const LOG_ERR}
fromPriority Warning   = #{const LOG_WARNING}
fromPriority Notice    = #{const LOG_NOTICE}
fromPriority Info      = #{const LOG_INFO}
fromPriority Debug     = #{const LOG_DEBUG}
