#include <syslog.h>

/*
   A variant of syslog(3) that provides a simplified API to address the
   following issues:

   - Calling variadic functions via FFI kind-of "just works", but the Haskell
     standard actually doesn't guarantee that it does. There's a GHC extension,
     CApiFFI, which addresses this issue, but that extension isn't part of any
     Haskell standard either.

   - Strings in Haskell are almost never terminated by a \0 byte, but we tend
     to know their length, so it's more convenient (and more efficient) to
     specify an explicit maximum field length in the format string via "%.*s"
     and pass our string as an argument to that. A welcome side-effect of this
     approach is that the call won't try to interpret any of those freaky %
     formatting features that we can't support (and don't want to, really).

   Note that this function makes no effort to verify the validity of its
   arguments. If you want to pass a negative string length, null pointers, and
   non-existent facilities here ... have at it and see what happens!
 */

void simpleSyslog(int facility, int priority, const char * buf, int len)
{
  syslog(facility | priority, "%.*s", len, buf);
}
