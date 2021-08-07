module Repld.Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket_, catchAny, throwIO)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as ByteString
import Data.Char (isSpace)
import Data.Function (fix, (&))
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Ki
import Network.Socket
import Network.Socket.ByteString (recv)
import RepldCommon
import qualified System.Console.ANSI as Console
import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Terminal.Size as Console
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)
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

  withRepldSocket \sock ->
    withRepl command \repl -> do
      Ki.scoped \scope -> do
        -- Accept clients forever. Handle one connected client's socket by forwarding all lines to the repl. Although
        -- the input is on a stream socket, we assume we receive an entire "request" in one 8K read.
        -- TODO wire format
        Ki.fork_ scope do
          forever do
            (client, _addr) <- accept sock
            -- Ephemeral background thread for each client - don't care if/how it dies.
            (void . forkIO) do
              fix \loop -> do
                bytes <- recv client 8192
                unless (ByteString.null bytes) do
                  text <- either throwIO pure (Text.decodeUtf8' bytes)
                  handleClientInput repl text
                  loop
        -- Forward our stdin into the repl. This makes the foregrounded repld server interactive.
        Ki.fork_ scope do
          let loop :: Haskeline.InputT IO ()
              loop =
                whenJustM (Haskeline.getInputLine "") \line -> do
                  liftIO (writelnRepl repl (Text.pack line))
                  loop
          Haskeline.runInputT Haskeline.defaultSettings loop
        -- Forward repl's stdout to our stdout.
        Ki.fork_ scope (forever (readRepl repl >>= Text.putStr))
        exitCode <- atomically (waitForReplToExit repl)
        exitWith exitCode
  where
    withRepldSocket :: (Socket -> IO a) -> IO a
    withRepldSocket action = do
      socketPath <- getRepldSocketPath
      whenM (doesFileExist socketPath) do
        hPutStrLn stderr ("The repld socket " ++ socketPath ++ " already exists.")
        hPutStrLn stderr "Perhaps repld is already running, or crashed?"
        exitWith (ExitFailure 1)
      withUnixSocketServer socketPath action

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

killRepl :: RunningRepl -> IO ()
killRepl =
  stopProcess . runningReplProcess

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

disableBuffering :: Handle -> IO ()
disableBuffering h =
  hSetBuffering h NoBuffering

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

withUnixSocketServer :: FilePath -> (Socket -> IO a) -> IO a
withUnixSocketServer path action =
  withUnixSocket \sock ->
    bracket_ (bind sock (SockAddrUnix path)) (ignoringExceptions (removeFile path)) do
      listen sock 5
      action sock

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mx = do
  b <- mb
  when b mx

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mx f =
  mx >>= \case
    Nothing -> pure ()
    Just x -> f x

whileM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileM mx f =
  fix \loop ->
    whenJustM mx (\x -> f x >> loop)

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

blue :: Console.Color
blue =
  Console.Blue

white :: Console.Color
white =
  Console.White

bg :: Console.ConsoleLayer
bg =
  Console.Background

fg :: Console.ConsoleLayer
fg =
  Console.Foreground
