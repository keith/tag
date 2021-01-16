module RunCommand where

import Command
import Data.Maybe (fromMaybe)
import System.IO
import System.Process

-- This function takes a Command argument, and launches a new process with the
-- given arguments. It then immediately returns the file handles to stdout and
-- stderr, along with the ProcessHandle. The in and out handles can be read
-- immediately.
runCommandWithoutBlocking :: Command -> IO (Handle, Handle, ProcessHandle)
runCommandWithoutBlocking (Command exec args _) = do
  (_, mout, merr, process) <-
    createProcess (proc exec args){
        std_out = CreatePipe
      , std_err = CreatePipe
    }

  let (hout, herr) = fromMaybe (error "Failed to create process")
                               ((,) <$> mout <*> merr)

  return (hout, herr, process)

-- This function takes a Command, and file handles to stdin, stdout, and stderr
-- and executes the command, reusing the file handles for the process. This
-- gives the child process complete control over how the data is read and
-- written to these Handles.
createProcessForCommand :: Command -> IO ProcessHandle
createProcessForCommand (Command exec args _) = do
  (_, _, _, process) <- createProcess (proc exec args)
  return process
