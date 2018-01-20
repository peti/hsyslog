{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: POSIX

   FFI bindings to @syslog(3)@ from
   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html POSIX.1-2008>.
   This module is intended for purposes of low-level implementation. Users of
   this library should prefer safer and more convenient API provided by
   "System.Posix.Syslog".
-}

module System.Posix.Syslog.LogMask where

import System.Posix.Syslog.Functions ( _logMask )
import System.Posix.Syslog.Priority ( Priority, fromPriority )

import Data.Bits
import Foreign.C.Types

-- | Convert a set of logging priorities into a system-dependent binary
-- representation suitable for calling '_setlogmask'.

toLogMask :: [Priority] -> CInt
toLogMask = foldr ((.|.) . _logMask . fromPriority) 0

-- | Decode the the system-dependent binary representation returned by
-- '_setlogmask' back into a set of logging priorities.

fromLogMask :: CInt -> [Priority]
fromLogMask old = [ p | p <- [minBound..maxBound], _logMask (fromPriority p) .&. old /= 0 ]
