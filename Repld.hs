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
import Control.Monad (forever, guard, join, unless, when, void)
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
import System.Exit (ExitCode(ExitFailure), exitWith)
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

-- | Repls we recognize.
data Repl
  = Ghci

main :: IO ()
main =
  join
    (Opt.customExecParser
      (Opt.prefs (Opt.showHelpOnError <> Opt.showHelpOnEmpty))
      (Opt.info (Opt.helper <*> parser) (Opt.progDesc "repld")))

  where
    parser :: Opt.Parser (IO ())
    parser =
      serveMain
        <$> Opt.strArgument (Opt.metavar "COMMAND")
        <*> Opt.switch
              (Opt.long "no-echo" <>
                Opt.help "Don't echo the input sent to the server")

serveMain :: String -> Bool -> IO ()
serveMain replCommand (not -> echo) =
  runManaged do
    disableBuffering stdout
    disableBuffering stderr

    socketPath :: FilePath <-
      getRepldSocketPath

    liftIO do
      whenM (doesFileExist socketPath) do
        hPutStrLn stderr ("The repld socket " ++ socketPath ++ " already exists.")
        hPutStrLn stderr "Perhaps repld is already running, or crashed?"
        exitWith (ExitFailure 1)

    sock :: Socket <-
      managedUnixSocketServer socketPath

    repl :: Process Handle Handle () <-
      managedProcess (replProcessConfig replCommand)

    let
      replStdin :: Handle
      replStdin =
        getStdin repl

    let
      handleClientInput :: Text -> IO ()
      handleClientInput (canonicalizeClientInput (parseRepl replCommand) -> input) = do
        Console.setCursorPosition 0 0
        Console.clearFromCursorToScreenEnd

        width :: Maybe Int <-
          fmap (\(Console.Window _ w) -> w) <$> Console.size

        (putStrLn . style [vivid white bg, vivid black fg])
          (case width of
            Nothing ->
              replCommand
            Just width ->
              replCommand ++ replicate (width - length replCommand) ' ')

        when echo do
          for_ input \line ->
            Text.putStrLn (Text.pack (style [vivid blue fg] "≫ ") <> line)

          for_ width \width ->
            Text.putStrLn (Text.replicate width "─")

        Text.hPutStr replStdin (Text.unlines input)

    let
      handleLocalInput :: Text -> IO ()
      handleLocalInput =
        Text.hPutStrLn replStdin

    disableBuffering replStdin

    doneInitializingVar :: MVar () <-
      liftIO newEmptyMVar

    acceptAsync :: Async () <-
      managedAsync (acceptThread (accept sock) handleClientInput)

    stdinAsync :: Async () <-
      managedAsync (stdinThread handleLocalInput)

    replStdoutAsync :: Async () <-
      managedAsync
        (replStdoutThread
          (putMVar doneInitializingVar ())
          (getStdout repl))

    -- Fake client input to clear the screen and such
    liftIO (takeMVar doneInitializingVar)
    liftIO (handleClientInput "")

    (liftIO . ignoringExceptions . atomically . asum)
      [ waitSTM acceptAsync
      , waitSTM stdinAsync
      , waitSTM replStdoutAsync
      , void (waitExitCodeSTM repl)
      ]

    stopProcess repl

  where
    parseRepl :: String -> Maybe Repl
    parseRepl command =
      asum
        [ Ghci <$ guard ("cabal " `isPrefixOf` command)
        , Ghci <$ guard ("ghci" == command)
        , Ghci <$ guard ("ghci "  `isPrefixOf` command)
        , Ghci <$ guard ("stack " `isPrefixOf` command)
        ]

replProcessConfig :: String -> ProcessConfig Handle Handle ()
replProcessConfig command =
  shell command
    & setDelegateCtlc True
    & setStdin createPipe
    & setStdout createPipe

-- | Canonicalize client input by:
--
--   * Removing leading line-comment markers (so input can be sent from within
--     a comment, and not ignored by the repl).
--   * Removing leading whitespace (but retaining alignment)
--   * Removing trailing whitespace
canonicalizeClientInput :: Maybe Repl -> Text -> [Text]
canonicalizeClientInput repl text = do
  block :: [Text] <-
    splitOn [""] (map uncomment (Text.lines text))

  let
    leadingWhitespace :: Int =
      -- minimum is safe here (for now...) because if block is null, we don't
      -- force this value
      minimum (map (Text.length . Text.takeWhile isSpace) block)

  let
    strip :: Text -> Text =
      Text.stripEnd . Text.drop leadingWhitespace

  filter (not . Text.null) (map strip block)

  where
    uncomment :: Text -> Text
    uncomment =
      case repl of
        Nothing ->
          id

        Just Ghci ->
          \s -> fromMaybe s (Text.stripPrefix "-- " s)

-- | Accept clients forever.
acceptThread
  :: IO (Socket, SockAddr)
  -> (Text -> IO ())
  -> IO ()
acceptThread acceptClient handleInput =
  forever do
    (clientSock, _clientSockAddr) <-
      acceptClient

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
  :: (Text -> IO ())
  -> IO ()
stdinThread handleInput =
  Haskeline.runInputT Haskeline.defaultSettings loop

  where
    loop :: Haskeline.InputT IO ()
    loop = do
      Haskeline.getInputLine "" >>= \case
        Nothing ->
          pure ()

        Just line -> do
          liftIO (handleInput (Text.pack line))
          loop

replStdoutThread
  :: IO ()
  -> Handle
  -> IO ()
replStdoutThread doneInitializing replStdout = do
  disableBuffering replStdout

  -- In the "initialization phase", expect some output every 1s
  initializationPhase `finally` doneInitializing

  -- Then, notify that we've initialized, and proceed to forward all repl stdout
  -- to the terminal.
  forever (Text.hGetChunk replStdout >>= Text.putStr)

  where
    initializationPhase :: IO ()
    initializationPhase =
      whileM
        (timeout 1_000_000 (Text.hGetChunk replStdout))
        Text.putStr

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
