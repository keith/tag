module Regex where

import Text.Regex.PCRE

getCaptureGroups :: String -> String -> [String]
getCaptureGroups string regex = do
  let (_, _, _, matches) = (string =~ regex :: (String, String, String, [String]))
  matches
