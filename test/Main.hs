module Main (main) where

import System.Posix.Syslog

{--
  This isn't a true test suite. Instead, we're passing the PERROR option
 (meaning syslog will also send messages to STDERR), sending a message that
 should be whitelisted by the priority mask, and sending a message that should
 be blacklisted by the priority mask. If hsyslog is working correct, then only
 "hsyslog is working" should appear in your test log output.
--}

main :: IO ()
main =
    withSyslog "hsyslog" options facility priority $ do
      syslogTo [MAIL, NEWS] [Debug, Alert] "hsyslog is working :)"
      syslog [Error] "hsyslog is not working :("
  where
    options = [PERROR, NDELAY]
    facility = [USER]
    priority = (Mask [Debug, Alert])
