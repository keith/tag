module Regex where

import Data.Tuple.Select
import Text.Regex.PCRE

getCaptureGroups :: String -> String -> [String]
getCaptureGroups string regex =
  sel4 (string =~ regex :: (String, String, String, [String]))
