module Vim where

import EditorPosition

vimEditCommand :: String -> (Line, Column) -> String
vimEditCommand path (Line line, Column column) = "vim "
  ++ "\\\"" ++ path ++ "\\\""
  ++ " \\\"+call cursor(" ++ show line ++ ", " ++ show column ++ ")\\\""
