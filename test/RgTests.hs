module RgTests (allRgTests) where

import Command
import EditorPosition
import LineType
import Rg
import Test.HUnit

testRgCommand :: Test
testRgCommand = TestCase $
  assertEqual "Passed arguments are appended to Command"
  (rgCommand ["foo"])
  (Command "rg" ["--heading", "--color", "always", "--column", "foo"])

testParsingRgFilePathOutput :: String -> Test
testParsingRgFilePathOutput line = TestCase $
  assertEqual "Test rg filepath line is parsed correctly"
  (getOutputType line)
  (FilePath "LICENSE")

testParsingRgSearchLine :: String -> Test
testParsingRgSearchLine line = TestCase $
  assertEqual "Test rg search output line is parsed correctly"
  (getOutputType line)
  (Location (Line 4) (Column 2))

allRgTests :: IO [Test]
allRgTests = do
  rgOutput <- readFile "test/fixtures/rgoutput.txt"

  return [
      testRgCommand
    , testParsingRgFilePathOutput $ head $ lines rgOutput
    , testParsingRgSearchLine $ lines rgOutput !! 1
    ]
