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

module System.Posix.Syslog.Facility where

import Foreign.C.Types
import GHC.Generics ( Generic )

#include <syslog.h>

-- | Syslog distinguishes various system facilities. Most applications should
-- log in 'USER'.

data Facility  = Kernel      -- ^ kernel messages
               | User        -- ^ user-level messages (default unless set otherwise)
               | Mail        -- ^ mail system
               | News        -- ^ network news subsystem
               | UUCP        -- ^ UUCP subsystem
               | Daemon      -- ^ system daemons
               | Auth        -- ^ security and authorization messages
               | Cron        -- ^ clock daemon
               | LPR         -- ^ line printer subsystem
               | Local0      -- ^ reserved for local use
               | Local1      -- ^ reserved for local use
               | Local2      -- ^ reserved for local use
               | Local3      -- ^ reserved for local use
               | Local4      -- ^ reserved for local use
               | Local5      -- ^ reserved for local use
               | Local6      -- ^ reserved for local use
               | Local7      -- ^ reserved for local use
   deriving (Show, Read, Bounded, Enum, Eq, Generic)

-- | Translate a 'Facility' into the system-dependent identifier that's used by
-- the @syslog(3)@ implementation.

{-# INLINE fromFacility #-}
fromFacility :: Facility -> CInt
fromFacility Kernel    = #{const LOG_KERN}
fromFacility User      = #{const LOG_USER}
fromFacility Mail      = #{const LOG_MAIL}
fromFacility Daemon    = #{const LOG_DAEMON}
fromFacility Auth      = #{const LOG_AUTH}
fromFacility LPR       = #{const LOG_LPR}
fromFacility News      = #{const LOG_NEWS}
fromFacility UUCP      = #{const LOG_UUCP}
fromFacility Cron      = #{const LOG_CRON}
fromFacility Local0    = #{const LOG_LOCAL0}
fromFacility Local1    = #{const LOG_LOCAL1}
fromFacility Local2    = #{const LOG_LOCAL2}
fromFacility Local3    = #{const LOG_LOCAL3}
fromFacility Local4    = #{const LOG_LOCAL4}
fromFacility Local5    = #{const LOG_LOCAL5}
fromFacility Local6    = #{const LOG_LOCAL6}
fromFacility Local7    = #{const LOG_LOCAL7}
