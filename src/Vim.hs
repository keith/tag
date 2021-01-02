module Vim where

import EditorPosition

vimEditCommand :: String -> Maybe (Line, Column) -> String
vimEditCommand path (Just (line, column)) = "eval '$EDITOR "
  ++ "\\\"" ++ path ++ "\\\""
  ++ " \\\"+call cursor(" ++ show line ++ ", " ++ show column ++ ")\\\"'"
vimEditCommand path Nothing = "eval '$EDITOR " ++ "\\\"" ++ path ++ "\\\"'"
