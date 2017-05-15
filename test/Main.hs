module Main (main) where

import Test.DocTest
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  distDir <- fromMaybe "dist" `fmap` lookupEnv "HASKELL_DIST_DIR"
  let hscFilesDir = distDir ++ "/build"
      packageDB = distDir ++ "/package.conf.inplace"
  doctest [ "-i" ++ hscFilesDir, "-package-db=" ++ packageDB, "-package=hsyslog", "src" ]
