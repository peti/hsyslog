{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Posix.Syslog

{--
  This isn't a true test suite. Instead, we're passing the PERROR option
 (meaning syslog will also send messages to STDERR), sending a message that
 should be whitelisted by the priority mask, and sending a message that should
 be blacklisted by the priority mask. If hsyslog is working correctly, then
 only "hsyslog is working" should appear in your test log output.
--}

config :: SyslogConfig
config = defaultConfig
    { options = [PERROR, NDELAY]
    , priorityMask = Mask [Debug, Alert]
    }

main :: IO ()
main = withSyslogTo config $ \syslogTo -> do
    syslogTo [NEWS] [Debug, Error] "%s%d hsyslog is working :)"
    syslogTo [MAIL] [Error] "hsyslog is not working :("
