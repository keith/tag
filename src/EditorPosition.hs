module EditorPosition where

newtype Line = Line Int
  deriving (Eq, Show)
newtype Column = Column Int
  deriving (Eq, Show)
