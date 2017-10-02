module Command where

data Command = Command {
    executable :: String
  , arguments  :: [String]
  } deriving (Show)
