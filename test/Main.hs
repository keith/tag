module Main where

import AgTests
import RgTests
import Test.HUnit

main :: IO Counts
main = do
  agTests <- allAgTests
  rgTests <- allRgTests
  runTestTT $ TestList $ agTests ++ rgTests
