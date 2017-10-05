module Ag where

import Command

agCommand :: [String] -> Command
agCommand args = Command "ag" $ ["--group", "--color", "--column"] ++ args
