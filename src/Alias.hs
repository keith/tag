module Alias where

aliasForCommand :: (Int, String) -> String
aliasForCommand (index, string) =
  "alias e" ++ show index ++ "=\"" ++ string ++ "\""
