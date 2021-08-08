module Repld.Main
  ( repld,
    repldSend,
  )
where

import Control.Exception.Safe (catchAny, tryAny)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Ki
import Repld.Prelude
import Repld.Server (runServer)
import qualified Repld.Socket as Socket
import qualified System.Console.ANSI as Console
import qualified System.Console.Haskeline as Haskeline
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO
import qualified System.Process.Typed as Process

--------------------------------------------------------------------------------
-- repld
-------------------------------------------------------------------------------

repld :: IO ()
repld = do
  command <-
    getArgs >>= \case
      [command] -> pure command
      _ -> exitFailure

  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  socketPath <- getRepldSocketPath

  -- A server socket may have been left around from a previous repld run that did not exit gracefully.
  whenM (doesFileExist socketPath) do
    tryAny (Socket.connect socketPath \_ -> pure ()) >>= \case
      Left _ -> pure () -- it's dead, jim
      Right () -> do
        hPutStrLn stderr "repld is already running."
        exitFailure

  let replConfig :: Process.ProcessConfig Handle () ()
      replConfig =
        Process.shell command
          & Process.setDelegateCtlc True
          & Process.setStdin Process.createPipe

  Process.withProcessWait replConfig \repl -> do
    hSetBuffering (Process.getStdin repl) NoBuffering

    Ki.scoped \scope -> do
      Ki.fork_ scope do
        runServer socketPath \case
          Socket.Frame "send" bytes -> do
            Console.setCursorPosition 0 0
            Console.clearFromCursorToScreenEnd
            Text.hPutStr (Process.getStdin repl) bytes
            pure (Socket.Frame "send" "")
          _ -> pure (Socket.Frame "error" "unrecognized frame")
      Ki.fork_ scope do
        let loop :: Haskeline.InputT IO ()
            loop =
              whenJustM (Haskeline.getInputLine "") \line -> do
                liftIO (Text.hPutStrLn (Process.getStdin repl) (Text.pack line))
                loop
        Haskeline.runInputT Haskeline.defaultSettings loop
        hClose (Process.getStdin repl)
      exitCode <- Process.waitExitCode repl
      exitWith exitCode

--------------------------------------------------------------------------------
-- repld-send
-------------------------------------------------------------------------------

repldSend :: IO ()
repldSend = do
  socketPath <- getRepldSocketPath
  (`catchAny` \_ -> exitFailure) do
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
