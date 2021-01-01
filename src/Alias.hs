module Alias where

aliasForCommand :: Int -> String -> String
aliasForCommand index string =
  "alias e" ++ show index ++ "=\"" ++ string ++ "\""

globalAliasForCommand :: Int -> String -> String
globalAliasForCommand index string =
  "alias -g f" ++ show index ++ "=\"" ++ string ++ "\""
