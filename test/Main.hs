module Main where

import AgTests
import Test.HUnit

main :: IO Counts
main = do
  agTests <- allAgTests
  runTestTT $ TestList agTests
