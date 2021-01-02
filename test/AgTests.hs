module AgTests (allAgTests) where

import Ag
import Command
import LineType
import Test.HUnit

testAgCommand :: Test
testAgCommand = TestCase $
  assertEqual "Passed arguments are appended to Command"
  (Command "ag" ["--group", "--color", "--column", "foo"] False)
  (agCommand ["foo"])

testParsingAgFilePathOutput :: String -> Test
testParsingAgFilePathOutput line = TestCase $
  assertEqual "Test ag filepath line is parsed correctly"
  (FilePath "LICENSE")
  (getOutputType line)

testParsingAgSearchLine :: String -> Test
testParsingAgSearchLine line = TestCase $
  assertEqual "Test ag search output line is parsed correctly"
  (Location 4 2)
  (getOutputType line)

allAgTests :: IO [Test]
allAgTests = do
  agOutput <- readFile "test/fixtures/agoutput.txt"

  return [
      testAgCommand
    , testParsingAgFilePathOutput $ head $ lines agOutput
    , testParsingAgSearchLine $ lines agOutput !! 1
    ]
