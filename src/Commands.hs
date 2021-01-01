module Commands where

import Ag (agCommand)
import Command
import Rg (rgCommand)

commandFromString :: String -> [String] -> Either String Command
commandFromString tool args
    | tool == "ag" = Right $ agCommand args
    | tool == "rg" = Right $ rgCommand args
    | otherwise = Left $ "'" ++ tool ++ "' is not a supported command"
