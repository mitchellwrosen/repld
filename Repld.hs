{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import RepldCommon

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, waitSTM, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe (bracket_, catchAny, finally, throwIO)
import Control.Monad (forever, guard, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed, runManaged)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Foldable (asum, for_)
import Data.Function ((&), fix)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Socket
import Network.Socket.ByteString (recv)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO
import System.Process.Typed
import System.Timeout (timeout)

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative as Opt
import qualified System.Console.ANSI as Console
import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Terminal.Size as Console

main :: IO ()
main = do
  program :: Managed () <-
    (Opt.customExecParser
      (Opt.prefs (Opt.showHelpOnError <> Opt.showHelpOnEmpty))
      (Opt.info (Opt.helper <*> mainParser) (Opt.progDesc "repld")))

  runManaged program

mainParser :: Opt.Parser (Managed ())
mainParser =
  theMain
    <$> Opt.strArgument (Opt.metavar "COMMAND")
    <*> Opt.switch
          (Opt.long "no-echo" <>
            Opt.help "Don't echo the input sent to the server")

theMain
  :: String -- ^ Shell command to run (should be a repl)
  -> Bool -- ^ Don't echo input sent?
  -> Managed ()
theMain replCommand (not -> echo) = do
  disableBuffering stdout
  disableBuffering stderr

  sock :: Socket <-
    bindRepldSocket

  repl :: RunningRepl <-
    spawnRepl replCommand

  doneInitializingVar :: MVar () <-
    liftIO newEmptyMVar

  acceptAsync :: Async () <-
    managedAsync
      (acceptThread
        sock
        (handleClientInput echo repl))

  stdinAsync :: Async () <-
    managedAsync (stdinThread repl)

  replStdoutAsync :: Async () <-
    managedAsync
      (replStdoutThread
        (putMVar doneInitializingVar ())
        repl)

  -- Fake client input to clear the screen and such
  liftIO (takeMVar doneInitializingVar)
  liftIO (handleClientInput echo repl "")

  (liftIO . ignoringExceptions . atomically . asum)
    [ waitSTM acceptAsync
    , waitSTM stdinAsync
    , waitSTM replStdoutAsync
    , void (waitForReplToExit repl)
    ]

  killRepl repl

bindRepldSocket :: Managed Socket
bindRepldSocket = do
  socketPath :: FilePath <-
    getRepldSocketPath

  liftIO do
    whenM (doesFileExist socketPath) do
      hPutStrLn stderr ("The repld socket " ++ socketPath ++ " already exists.")
      hPutStrLn stderr "Perhaps repld is already running, or crashed?"
      exitWith (ExitFailure 1)

  managedUnixSocketServer socketPath

handleClientInput
  :: Bool -- ^ Echo client input?
  -> RunningRepl -- ^ Running repl
  -> Text -- ^ Client input
  -> IO ()
handleClientInput echo repl (canonicalizeClientInput -> input) = do
  clearTerminal
  width <- getConsoleWidth
  putReplCommand width
  putClientInput width
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
        (runningReplCommand repl ++
          replicate (width - length (runningReplCommand repl)) ' ')

    putClientInput :: Int -> IO ()
    putClientInput width =
      when echo do
        for_ input \line ->
          Text.putStrLn (Text.pack (style [vivid blue fg] "≫ ") <> line)
        Text.putStrLn (Text.replicate width "─")

-- | Canonicalize client input by:
--
--   * Removing leading whitespace (but retaining alignment)
--   * Removing trailing whitespace
canonicalizeClientInput :: Text -> [Text]
canonicalizeClientInput text = do
  block :: [Text] <-
    splitOn [""] (Text.lines text)

  let
    leadingWhitespace :: Int =
      -- minimum is safe here (for now...) because if block is null, we don't
      -- force this value
      minimum (map (Text.length . Text.takeWhile isSpace) block)

  let
    strip :: Text -> Text =
      Text.stripEnd . Text.drop leadingWhitespace

  filter (not . Text.null) (map strip block)

-- | Accept clients forever.
acceptThread
  :: Socket
  -> (Text -> IO ())
  -> IO ()
acceptThread sock handleInput =
  forever do
    (clientSock, _clientSockAddr) <-
      accept sock

    -- Ephemeral background thread for each client - don't care if/how it dies.
    void (forkIO (clientThread clientSock handleInput))

-- | Handle one connected client's socket by forwarding all lines to the repl.
-- Although the input is on a stream socket, I am assuming that I receive an
-- entire "request" in one 8K read.
clientThread
  :: Socket
  -> (Text -> IO ())
  -> IO ()
clientThread sock handleInput =
  fix \loop -> do
    bytes :: ByteString <-
      recv sock 8192

    unless (ByteString.null bytes) do
      text :: Text <-
        either throwIO pure (Text.decodeUtf8' bytes)
      handleInput text
      loop

-- | Forward stdin to this process onto the repl. This makes the foregrounded
-- repld server interactive.
stdinThread
  :: RunningRepl
  -> IO ()
stdinThread repl =
  Haskeline.runInputT Haskeline.defaultSettings loop

  where
    loop :: Haskeline.InputT IO ()
    loop = do
      Haskeline.getInputLine "" >>= \case
        Nothing ->
          pure ()

        Just line -> do
          liftIO (writelnRepl repl (Text.pack line))
          loop

replStdoutThread
  :: IO ()
  -> RunningRepl
  -> IO ()
replStdoutThread doneInitializing repl = do
  -- In the "initialization phase", expect some output every 1s
  initializationPhase `finally` doneInitializing

  -- Then, notify that we've initialized, and proceed to forward all repl stdout
  -- to the terminal.
  forever (readRepl repl >>= Text.putStr)

  where
    initializationPhase :: IO ()
    initializationPhase =
      whileM
        (timeout 1_000_000 (readRepl repl))
        Text.putStr


--------------------------------------------------------------------------------
-- Running repl
-------------------------------------------------------------------------------

-- | Repls we recognize.
data ReplType
  = Ghci
  | UnknownRepl

stripLineComment
  :: ReplType
  -> Text
  -> Text
stripLineComment = \case
  Ghci -> \s -> fromMaybe s (Text.stripPrefix "-- " s)
  UnknownRepl -> id

-- | Crudely try to figure out what repl is being spawned. Totally inaccurate as
-- it cannot see thru aliases/scripts. TODO add a flag that tells repld what
-- repl is being spawned.
parseReplType :: String -> ReplType
parseReplType command =
  fromMaybe UnknownRepl
    (asum
      [ Ghci <$ guard ("cabal " `isPrefixOf` command)
      , Ghci <$ guard ("ghci" == command)
      , Ghci <$ guard ("ghci "  `isPrefixOf` command)
      , Ghci <$ guard ("stack " `isPrefixOf` command)
      ])


data RunningRepl
  = RunningRepl
  { runningReplCommand :: String
  , runningReplType :: ReplType
  , runningReplProcess :: Process Handle Handle ()
  }

spawnRepl
  :: String
  -> Managed RunningRepl
spawnRepl command = do
  process <- managedProcess config
  disableBuffering (getStdin process)
  disableBuffering (getStdout process)
  pure RunningRepl
    { runningReplCommand = command
    , runningReplType = parseReplType command
    , runningReplProcess = process
    }
  where
    config :: ProcessConfig Handle Handle ()
    config =
      shell command
        & setDelegateCtlc True
        & setStdin createPipe
        & setStdout createPipe

readRepl
  :: RunningRepl
  -> IO Text
readRepl =
  Text.hGetChunk . getStdout . runningReplProcess

writeRepl
  :: RunningRepl
  -> Text
  -> IO ()
writeRepl repl =
  writeReplRaw repl . stripLineComment (runningReplType repl)

writeReplRaw
  :: RunningRepl
  -> Text
  -> IO ()
writeReplRaw =
  Text.hPutStr . getStdin . runningReplProcess

writelnRepl
  :: RunningRepl
  -> Text
  -> IO ()
writelnRepl repl =
  writelnReplRaw repl . stripLineComment (runningReplType repl)

writelnReplRaw
  :: RunningRepl
  -> Text
  -> IO ()
writelnReplRaw =
  Text.hPutStrLn . getStdin . runningReplProcess

waitForReplToExit
  :: RunningRepl
  -> STM ExitCode
waitForReplToExit =
  waitExitCodeSTM . runningReplProcess

killRepl
  :: MonadIO m
  => RunningRepl
  -> m ()
killRepl =
  stopProcess . runningReplProcess

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

disableBuffering :: MonadIO m => Handle -> m ()
disableBuffering h =
  liftIO (hSetBuffering h NoBuffering)

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

managedAsync :: IO a -> Managed (Async a)
managedAsync action =
  managed (withAsync action)

managedProcess :: ProcessConfig a b c -> Managed (Process a b c)
managedProcess config =
  managed (withProcessWait config)

managedUnixSocketServer :: FilePath -> Managed Socket
managedUnixSocketServer path = do
  sock :: Socket <-
    managedUnixSocket

  managed
    (\k ->
      bracket_
        (bind sock (SockAddrUnix path))
        (ignoringExceptions (removeFile path))
        (do
          listen sock 5
          k sock))

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

vivid
  :: Console.Color
  -> Console.ConsoleLayer
  -> Console.SGR
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
