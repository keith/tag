module Rg where

import Command

rgCommand :: [String] -> Command
rgCommand args = Command
  "rg" (["--heading", "--color", "always", "--column"] ++ args) False
