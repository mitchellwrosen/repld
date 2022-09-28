module Repld.Main
  ( repld,
    repldSend,
  )
where

import Control.Exception.Safe (bracket, catchAny, tryAny)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Ki qualified
import Repld.App
import Repld.Prelude
import Repld.Server (runServer)
import Repld.Socket qualified as Socket
import System.Console.Haskeline qualified as Haskeline
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO
import System.Process qualified as Process
import Prelude hiding (return)

repld :: IO ()
repld =
  runApp () app >>= \case
    0 -> pure ()
    code -> exitWith (ExitFailure code)

app :: App x r Int
app = do
  label \return -> do
    command <-
      io getArgs >>= \case
        [command] -> pure command
        _ -> return 1

    io (hSetBuffering stdout NoBuffering)
    io (hSetBuffering stderr NoBuffering)

    socketPath <- io getRepldSocketPath

    -- A server socket may have been left around from a previous repld run that did not exit gracefully.
    whenM (io (doesFileExist socketPath)) do
      io (tryAny (Socket.connect socketPath \_ -> pure ())) >>= \case
        Left _ -> pure () -- it's dead, jim
        Right () -> do
          io (hPutStrLn stderr "repld is already running.")
          return 1

    with (withRepl command) \repl -> do
      with Ki.scoped \scope -> do
        io do
          Ki.fork_ scope do
            runServer socketPath \case
              Socket.Frame "send" string -> do
                Text.putStr string
                sendToRepl repl string
                pure (Socket.Frame "send" "")
              _ -> pure (Socket.Frame "error" "unrecognized frame")
        _ <-
          io do
            Ki.fork @() scope do
              let loop :: Haskeline.InputT IO ()
                  loop =
                    whenJustM (Haskeline.getInputLine "") \line -> do
                      liftIO (sendToRepl repl (Text.pack line <> "\n"))
                      loop
              Haskeline.runInputT Haskeline.defaultSettings loop
              closeReplStdin repl
        io (getReplExitCode repl) >>= \case
          ExitSuccess -> pure 0
          ExitFailure _code -> return 1

data Repl = Repl
  { replHandle :: Process.ProcessHandle,
    replStdin :: Handle
  }

withRepl :: String -> (Repl -> IO a) -> IO a
withRepl command action =
  bracket acquire release action
  where
    acquire :: IO Repl
    acquire = do
      (Just replStdin, _, _, replHandle) <-
        Process.createProcess
          (Process.shell command)
            { Process.delegate_ctlc = True,
              Process.std_in = Process.CreatePipe
            }
      hSetBuffering replStdin NoBuffering
      pure Repl {replHandle, replStdin}

    release :: Repl -> IO ()
    release Repl {replHandle} =
      Process.terminateProcess replHandle

sendToRepl :: Repl -> Text -> IO ()
sendToRepl Repl {replStdin} =
  Text.hPutStr replStdin

closeReplStdin :: Repl -> IO ()
closeReplStdin Repl {replStdin} =
  hClose replStdin

getReplExitCode :: Repl -> IO ExitCode
getReplExitCode Repl {replHandle} =
  Process.waitForProcess replHandle

repldSend :: IO ()
repldSend = do
  (`catchAny` \_ -> exitFailure) do
    socketPath <- getRepldSocketPath
    Socket.connect socketPath \sock -> do
      text <- Text.hGetContents stdin
      Socket.send sock (Socket.Frame "send" text)
      _ <- Socket.recv sock
      pure ()

getRepldSocketPath :: IO FilePath
getRepldSocketPath = do
  repldDir <-
    lookupEnv "XDG_RUNTIME_DIR" >>= \case
      Nothing -> getXdgDirectory XdgData "repld"
      Just dir -> pure (dir </> "repld")
  createDirectoryIfMissing True repldDir
  pure (repldDir </> "repld")
