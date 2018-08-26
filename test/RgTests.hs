module RgTests (allRgTests) where

import Command
import LineType
import Rg
import Test.HUnit

testRgCommand :: Test
testRgCommand = TestCase $
  assertEqual "Passed arguments are appended to Command"
  (Command "rg" ["--heading", "--color", "always", "--column", "foo"])
  (rgCommand ["foo"])

testParsingRgFilePathOutput :: String -> Test
testParsingRgFilePathOutput line = TestCase $
  assertEqual "Test rg filepath line is parsed correctly"
  (FilePath "LICENSE")
  (getOutputType line)

testParsingRgSearchLine :: String -> Test
testParsingRgSearchLine line = TestCase $
  assertEqual "Test rg search output line is parsed correctly"
  (Location 4 2)
  (getOutputType line)

allRgTests :: IO [Test]
allRgTests = do
  rgOutput <- readFile "test/fixtures/rgoutput.txt"

  return [
      testRgCommand
    , testParsingRgFilePathOutput $ head $ lines rgOutput
    , testParsingRgSearchLine $ lines rgOutput !! 1
    ]
