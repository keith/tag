module CustomIO where

import System.IO (Handle, hIsEOF, hGetLine)

-- This function takes a function, a file handle, and a piece of context, and
-- incrementally reads the file. With each line it passes the string to the
-- handle function, along with the previous context, in order to get a new
-- context. This new context is passed to the function when the next line is
-- read, and so on. When the EOF is reached, the function returns
processFileHandle :: (String -> Maybe a -> IO (Maybe a))
                     -> Handle -> Maybe a -> IO ()
processFileHandle func handle context = do
  eof <- hIsEOF handle
  if eof
    then return ()
    else do
      line <- hGetLine handle
      newContext <- func line context
      processFileHandle func handle newContext
