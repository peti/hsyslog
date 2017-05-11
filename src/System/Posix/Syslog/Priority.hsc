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

data Priority = Emergency       -- ^ the system is unusable
              | Alert           -- ^ action must be taken immediately
              | Critical        -- ^ critical conditions
              | Error           -- ^ error conditions
              | Warning         -- ^ warning conditions
              | Notice          -- ^ normal but significant condition
              | Info            -- ^ informational
              | Debug           -- ^ debug-level messages
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
