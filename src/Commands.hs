module Commands where

import Ag (agCommand)
import Command

commandFromString :: String -> Either String ([String] -> Command)
commandFromString string
    | string == "ag" = Right agCommand
    | otherwise = Left $ "'" ++ string ++ "' is not a supported command"
