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

module System.Posix.Syslog.Options where

import Foreign.C.Types
import GHC.Generics ( Generic )

#include <syslog.h>

-- | The function 'openlog' allows one to configure a handful of process-wide
-- options that modify the bahavior of the 'syslog' funcion. These options are
-- 'pid', 'cons', 'odelay', and 'ndelay'.

data Option = LogPID              -- ^ Log the pid with each message.
            | Console             -- ^ Log on the console if errors occur while sending messages.
            | DelayedOpen         -- ^ Delay all initialization until first @syslog()@ call (default).
            | ImmediateOpen       -- ^ Initalize the syslog system immediately.
            | DontWaitForChildren -- ^ The syslog system should not attempt to wait for child
                                  -- process it may have created. This option is required by
                                  -- applications who enable @SIGCHLD@ themselves.
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

-- | Translate an 'Option' into the system-dependent identifier that's used by
-- the @syslog(3)@ implementation.

{-# INLINE fromOption #-}
fromOption :: Option -> CInt
fromOption LogPID              = #{const LOG_PID}
fromOption Console             = #{const LOG_CONS}
fromOption DelayedOpen         = #{const LOG_ODELAY}
fromOption ImmediateOpen       = #{const LOG_NDELAY}
fromOption DontWaitForChildren = #{const LOG_NOWAIT}
