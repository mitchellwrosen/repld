module Repld.Main
  ( repld,
    repldSend,
  )
where

import Control.Exception.Safe (catchAny, tryAny)
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
import System.Process.Typed qualified as Process
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

    let replConfig :: Process.ProcessConfig Handle () ()
        replConfig =
          Process.shell command
            & Process.setDelegateCtlc True
            & Process.setStdin Process.createPipe

    with (Process.withProcessWait replConfig) \repl -> do
      let tellRepl = Text.hPutStr (Process.getStdin repl)
      let tellReplLn = Text.hPutStrLn (Process.getStdin repl)

      io (hSetBuffering (Process.getStdin repl) NoBuffering)

      with Ki.scoped \scope -> do
        io do
          Ki.fork_ scope do
            runServer socketPath \case
              Socket.Frame "send" bytes -> do
                Text.putStr bytes
                tellRepl bytes
                pure (Socket.Frame "send" "")
              _ -> pure (Socket.Frame "error" "unrecognized frame")
        _ <-
          io do
            Ki.fork @() scope do
              let loop :: Haskeline.InputT IO ()
                  loop =
                    whenJustM (Haskeline.getInputLine "") \line -> do
                      liftIO (tellReplLn (Text.pack line))
                      loop
              Haskeline.runInputT Haskeline.defaultSettings loop
              hClose (Process.getStdin repl)
        Process.waitExitCode repl >>= \case
          ExitSuccess -> pure 0
          ExitFailure _code -> return 1

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
