module Main where

import Command
import Commands (commandFromString)
import CustomIO (processFileHandle)
import Data.Maybe (listToMaybe)
import EitherUtils (maybeToEither)
import Parser (handleOutputLine)
import RunCommand (runCommandWithoutBlocking)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Process (waitForProcess)
import System.Posix.User (getLoginName)

runCommand :: Command -> IO ExitCode
runCommand command = do
  (hStdout, hStderr, proc) <- runCommandWithoutBlocking command
  hSetBinaryMode hStdout True

  userName <- getLoginName
  writeHandle <- openFile ("/tmp/tag_aliases_" ++ userName) WriteMode
  processFileHandle (handleOutputLine writeHandle) hStdout Nothing

  exitCode <- waitForProcess proc

  hGetContents hStderr >>= putStr
  hClose hStdout
  hClose hStderr
  hClose writeHandle

  return exitCode

main :: IO ExitCode
main = do
  args <- getArgs

  let commandOrError = maybeToEither
                       "No argument passed\n\nUsage: tag [ag|rg] ARGS"
                       (listToMaybe args) >>= commandFromString
  case commandOrError of
    Left string -> hPutStrLn stderr string >> exitWith (ExitFailure 1)
    Right command -> runCommand $ command $ tail args
