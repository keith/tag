module Commands where

import Ag (agCommand)
import Command
import Rg (rgCommand)

commandFromString :: String -> [String] -> Either String Command
commandFromString tool args
    | tool == "ag" = Right $ agCommand args
    | tool == "rg" = Right $ rgCommand args
    | tool == "fd" = Right $ Command "fd" args True
    | tool == "find" = Right $ Command "find" args True
    | otherwise = Left $ "'" ++ tool ++ "' is not a supported command"
