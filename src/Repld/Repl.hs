module Repld.Repl
  ( Repl,
    replCommand,
    runRepl,
    readRepl,
    writeRepl,
    writelnRepl,
    closeReplStdin,
    waitForReplToExit,
  )
where

import Control.Concurrent.STM
import qualified Data.Text.IO as Text
import Repld.Prelude
import System.Exit (ExitCode)
import System.IO
import System.Process.Typed

data Repl = Repl
  { command :: String,
    process :: Process Handle Handle ()
  }

runRepl :: String -> (Repl -> IO a) -> IO a
runRepl command action =
  withProcessWait config \process -> do
    hSetBuffering (getStdin process) NoBuffering
    hSetBuffering (getStdout process) NoBuffering
    action Repl {command, process}
  where
    config :: ProcessConfig Handle Handle ()
    config =
      shell command
        & setDelegateCtlc True
        & setStdin createPipe
        & setStdout createPipe

replCommand :: Repl -> String
replCommand =
  command

closeReplStdin :: Repl -> IO ()
closeReplStdin =
  hClose . getStdin . process

readRepl :: Repl -> IO Text
readRepl =
  Text.hGetChunk . getStdout . process

writeRepl :: Repl -> Text -> IO ()
writeRepl =
  Text.hPutStr . getStdin . process

writelnRepl :: Repl -> Text -> IO ()
writelnRepl =
  Text.hPutStrLn . getStdin . process

waitForReplToExit :: Repl -> STM ExitCode
waitForReplToExit =
  waitExitCodeSTM . process
