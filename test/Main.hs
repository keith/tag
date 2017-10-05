module Main where

import AgTests
import RgTests
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  agTests <- allAgTests
  rgTests <- allRgTests
  (Counts _ _ errorCount failureCount) <- runTestTT $
    TestList $ agTests ++ rgTests
  if errorCount > 0 || failureCount > 0 then exitFailure else exitSuccess
