#include <syslog.h>

/*
   A variant of syslog(3) that provides a simplified API to addresses the
   following issues:

   - Calling variadic functions via FFI kind-of sort-of works, but the Haskell
     standard actually doesn't guarantee it. There is a GHC extension, CApiFFI,
     which addresses this issue, but then that extension isn't part of any
     Haskell standard either.

   - Strings in Haskell are almost never terminated by a \0 byte. We generally
     do know their lengths, though, so it's more convenient to specify an
     explicit maximum field length in the format string via "%.*s" and pass our
     string as an argument to that. An added benefit is that our syslog
     function doesn't interpret any of those freaky % formatting features that
     can't support (and don't want to, really).

   Note that we totally don't make any effort whatsoever to guarantee
   meaningful argument values. If you wan to pass a negative string length,
   null pointers and non-existent  facility values ... be our guest!
 */

void simpleSyslog(int facility, int priority, const char * buf, int len)
{
  syslog(facility | priority, "%.*s", len, buf);
}
