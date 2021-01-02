module Command where

data Command = Command {
    executable :: String
  , arguments  :: [String]
  , includeFiles :: Bool
  } deriving (Eq, Show)
