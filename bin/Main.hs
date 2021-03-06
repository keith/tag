module Main where

import Command
import Commands (commandFromString)
import CustomIO (isRunningThroughPipes, processFileHandle)
import Data.List (isSuffixOf)
import Parser (handleOutputLine)
import RunCommand (createProcessForCommand, runCommandWithoutBlocking)
import System.Console.ParseArgs
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.IO
import System.Process (waitForProcess)

data Options =
    AliasFile |
    Tool
    deriving (Ord, Eq, Show)

argd :: [Arg Options]
argd = [
    Arg {
      argIndex = AliasFile,
      argName = Just "alias-file",
      argAbbr = Nothing,
      argData = argDataDefaulted "/tmp/tag_aliases" ArgtypeString "/tmp/tag_aliases",
      argDesc = "The output file path containing the aliases"
    },
    Arg {
      argIndex = Tool,
      argName = Nothing,
      argAbbr = Nothing,
      argData = argDataRequired "tool" ArgtypeString,
      argDesc = "[rg|ag]"
    }
  ]

getShell :: IO String
getShell = do
 shell <- lookupEnv "SHELL"
 return $ case shell of
   Just path -> if "zsh" `isSuffixOf` path then "zsh" else "unknown"
   Nothing -> "unknown"

runFilterCommand :: String -> Command -> IO ExitCode
runFilterCommand aliasFile command = do
  (hStdout, hStderr, proc) <- runCommandWithoutBlocking command
  hSetBinaryMode hStdout True

  writeHandle <- openFile aliasFile WriteMode
  shell <- getShell
  processFileHandle (handleOutputLine writeHandle shell (includeFiles command)) hStdout Nothing

  exitCode <- waitForProcess proc

  hGetContents hStderr >>= putStr
  hClose hStdout
  hClose hStderr
  hClose writeHandle

  return exitCode

runPassthroughCommand :: Command -> IO ExitCode
runPassthroughCommand command =
  createProcessForCommand command >>= waitForProcess

main :: IO ExitCode
main = do
  args <- parseArgsIO (ArgsParseControl (ArgsTrailing "passthrough arguments") ArgsSoftDash) argd
  let tool = getRequiredArg args Tool
  let otherArgs = argsRest args
  isPiped <- isRunningThroughPipes
  if isPiped
    then runPassthroughCommand $ Command tool otherArgs False
    else do
      command <- case commandFromString tool otherArgs of
        Left string -> hPutStrLn stderr string >> exitWith (ExitFailure 1)
        Right command -> return command
      runFilterCommand (getRequiredArg args AliasFile) command
