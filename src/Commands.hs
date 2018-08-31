module Commands where

import Ag (agCommand)
import Command
import Rg (rgCommand)

commandFromArguments :: [String] -> Command
commandFromArguments args = Command (head args) (tail args)

commandFromString :: String -> Either String ([String] -> Command)
commandFromString string
    | string == "ag" = Right agCommand
    | string == "rg" = Right rgCommand
    | otherwise = Left $ "'" ++ string ++ "' is not a supported command"
