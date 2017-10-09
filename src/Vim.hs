module Vim where

import EditorPosition

vimEditCommand :: String -> (Line, Column) -> String
vimEditCommand path (line, column) = "vim "
  ++ "\\\"" ++ path ++ "\\\""
  ++ " \\\"+call cursor(" ++ show line ++ ", " ++ show column ++ ")\\\""
