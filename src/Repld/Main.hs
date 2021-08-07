module Repld.Main
  ( repld,
    repldSend,
  )
where

import Control.Concurrent.STM
import Control.Exception.Safe (catchAny, throwIO)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Ki
import Repld.Prelude
import Repld.Server (runServer)
import qualified Repld.Socket as Socket
import qualified System.Console.ANSI as Console
import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Terminal.Size as Console
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO
import System.Process.Typed

repld :: IO ()
repld = do
  command <-
    getArgs >>= \case
      [command] -> pure command
      _ -> exitFailure

  disableBuffering stdout
  disableBuffering stderr

  socketPath <- getRepldSocketPath
  whenM (doesFileExist socketPath) do
    hPutStrLn stderr ("The repld socket " ++ socketPath ++ " already exists.")
    hPutStrLn stderr "Perhaps repld is already running, or crashed?"
    exitWith (ExitFailure 1)

  withRepl command \repl -> do
    Ki.scoped \scope -> do
      -- Handle client requests by forwarding all lines to the repl.
      Ki.fork_ scope do
        runServer socketPath \case
          Socket.Frame "send" bytes -> do
            handleClientInput repl bytes
            pure (Socket.Frame "send" "")
          _ -> pure (Socket.Frame "error" "unrecognized frame")
      -- Forward our stdin into the repl. This makes the foregrounded repld server interactive.
      Ki.fork_ scope do
        let loop :: Haskeline.InputT IO ()
            loop =
              whenJustM (Haskeline.getInputLine "") \line -> do
                liftIO (writelnRepl repl (Text.pack line))
                loop
        Haskeline.runInputT Haskeline.defaultSettings loop
        hClose (getStdin (runningReplProcess repl))
      -- Forward repl's stdout to our stdout.
      Ki.fork_ scope (forever (readRepl repl >>= Text.putStr))
      exitCode <- atomically (waitForReplToExit repl)
      exitWith exitCode

handleClientInput :: RunningRepl -> Text -> IO ()
handleClientInput repl (canonicalizeClientInput -> input) = do
  clearTerminal
  width <- getConsoleWidth
  putReplCommand width
  writeRepl repl (Text.unlines input)
  where
    clearTerminal :: IO ()
    clearTerminal = do
      Console.setCursorPosition 0 0
      Console.clearFromCursorToScreenEnd

    getConsoleWidth :: IO Int
    getConsoleWidth =
      Console.size >>= \case
        Nothing -> throwIO (userError "Failed to get console width")
        Just (Console.Window _ w) -> pure w

    putReplCommand :: Int -> IO ()
    putReplCommand width =
      (putStrLn . style [vivid white bg, vivid black fg])
        (runningReplCommand repl ++ replicate (width - length (runningReplCommand repl)) ' ')

-- | Canonicalize client input by:
--
--   * Removing leading whitespace (but retaining alignment)
--   * Removing trailing whitespace
canonicalizeClientInput :: Text -> [Text]
canonicalizeClientInput text = do
  block <- splitOn [""] (Text.lines text)

  let leadingWhitespace :: Int
      leadingWhitespace =
        -- minimum is safe here (for now...) because if block is null, we don't
        -- force this value
        minimum (map (Text.length . Text.takeWhile isSpace) block)

  let strip :: Text -> Text
      strip =
        Text.stripEnd . Text.drop leadingWhitespace

  filter (not . Text.null) (map strip block)

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

--------------------------------------------------------------------------------
-- Running repl
-------------------------------------------------------------------------------

data RunningRepl = RunningRepl
  { runningReplCommand :: String,
    runningReplProcess :: Process Handle Handle ()
  }

withRepl :: String -> (RunningRepl -> IO a) -> IO a
withRepl command action =
  withProcessWait config \process -> do
    disableBuffering (getStdin process)
    disableBuffering (getStdout process)
    action
      RunningRepl
        { runningReplCommand = command,
          runningReplProcess = process
        }
  where
    config :: ProcessConfig Handle Handle ()
    config =
      shell command
        & setDelegateCtlc True
        & setStdin createPipe
        & setStdout createPipe

readRepl :: RunningRepl -> IO Text
readRepl =
  Text.hGetChunk . getStdout . runningReplProcess

writeRepl :: RunningRepl -> Text -> IO ()
writeRepl =
  Text.hPutStr . getStdin . runningReplProcess

writelnRepl :: RunningRepl -> Text -> IO ()
writelnRepl =
  Text.hPutStrLn . getStdin . runningReplProcess

waitForReplToExit :: RunningRepl -> STM ExitCode
waitForReplToExit =
  waitExitCodeSTM . runningReplProcess

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getRepldSocketPath :: IO FilePath
getRepldSocketPath = do
  repldDir <-
    lookupEnv "XDG_RUNTIME_DIR" >>= \case
      Nothing -> getXdgDirectory XdgData "repld"
      Just dir -> pure (dir </> "repld")
  createDirectoryIfMissing True repldDir
  pure (repldDir </> "repld")

disableBuffering :: Handle -> IO ()
disableBuffering h =
  hSetBuffering h NoBuffering

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mx = do
  b <- mb
  when b mx

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mx f =
  mx >>= \case
    Nothing -> pure ()
    Just x -> f x

--------------------------------------------------------------------------------
-- Saner color API
--------------------------------------------------------------------------------

style :: [Console.SGR] -> String -> String
style code s =
  Console.setSGRCode code ++ s ++ Console.setSGRCode [Console.Reset]

vivid :: Console.Color -> Console.ConsoleLayer -> Console.SGR
vivid color layer =
  Console.SetColor layer Console.Vivid color

black :: Console.Color
black =
  Console.Black

white :: Console.Color
white =
  Console.White

bg :: Console.ConsoleLayer
bg =
  Console.Background

fg :: Console.ConsoleLayer
fg =
  Console.Foreground
