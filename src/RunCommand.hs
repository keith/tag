module RunCommand where

import Command
import Data.Maybe (fromMaybe)
import System.IO (Handle)
import System.Process

-- This function takes a Command argument, and launches a new process with the
-- given arguments. It then immediately returns the file handles to stdout and
-- stderr, along with the ProcessHandle. The in and out handles can be read
-- immediately.
runCommandWithoutBlocking :: Command -> IO (Handle, Handle, ProcessHandle)
runCommandWithoutBlocking (Command exec args) = do
  (_, mout, merr, process) <-
    createProcess (proc exec args){
        std_out = CreatePipe
      , std_err = CreatePipe
    }

  let (hout, herr) = fromMaybe (error "Failed to create process")
                               ((,) <$> mout <*> merr)

  return (hout, herr, process)
