{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, waitEitherSTM_, withAsync)
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket, bracket_, catchAny, throwIO)
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
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (XdgDirectory(..), doesFileExist, getXdgDirectory, removeFile)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO
import System.Process.Typed

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative as Opt
import qualified System.Console.ANSI as Console
import qualified System.Console.Terminal.Size as Console


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

--------------------------------------------------------------------------------
-- Send
--------------------------------------------------------------------------------

sendMain :: IO ()
sendMain =
  runManaged do
    socketPath :: FilePath <-
      getRepldSocketPath

    bytes :: ByteString <-
      liftIO (ByteString.hGetContents stdin)

    sock :: Socket <-
      managedUnixSocket

    liftIO
      (connect sock (SockAddrUnix socketPath) `catchAny` \_ ->
        exitWith (ExitFailure 1))

    liftIO (sendAll sock bytes)

-- ()

--------------------------------------------------------------------------------
-- Serve
--------------------------------------------------------------------------------

-- | Repls we recognize.
data Repl
  = Ghci

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

    replProcess :: Process Handle () () <-
      managedProcess replProcessConfig

    let
      replProcessStdin :: Handle
      replProcessStdin =
        getStdin replProcess

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

        Text.hPutStr replProcessStdin (Text.unlines input)

    let
      handleLocalInput :: Text -> IO ()
      handleLocalInput =
        Text.hPutStrLn replProcessStdin

    disableBuffering replProcessStdin

    acceptAsync :: Async () <-
      managedAsync (acceptThread (accept sock) handleClientInput)

    stdinAsync :: Async () <-
      managedAsync (stdinThread handleLocalInput)

    (liftIO . atomically)
      (waitEitherSTM_ acceptAsync stdinAsync <|>
        void (waitExitCodeSTM replProcess))

    stopProcess replProcess

  where
    replProcessConfig :: ProcessConfig Handle () ()
    replProcessConfig =
      shell replCommand
        & setDelegateCtlc True
        & setStdin createPipe

    parseRepl :: String -> Maybe Repl
    parseRepl command =
      asum
        [ Ghci <$ guard ("cabal " `isPrefixOf` command)
        , Ghci <$ guard ("ghci" == command)
        , Ghci <$ guard ("ghci "  `isPrefixOf` command)
        , Ghci <$ guard ("stack " `isPrefixOf` command)
        ]


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

-- | Forward stdin to this process onto the repl. This makes the foregrounded
-- repld server interactive.
stdinThread
  :: (Text -> IO ())
  -> IO ()
stdinThread handleInput =
  ignoringExceptions (forever (Text.getLine >>= handleInput))

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


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

disableBuffering :: MonadIO m => Handle -> m ()
disableBuffering h =
  liftIO (hSetBuffering h NoBuffering)

getRepldSocketPath :: MonadIO m => m FilePath
getRepldSocketPath =
  liftIO (getXdgDirectory XdgData "repld.sock")

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

managedUnixSocket :: Managed Socket
managedUnixSocket =
  managed
    (bracket
      (socket AF_UNIX Stream defaultProtocol)
      close)

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


--------------------------------------------------------------------------------
-- Saner color API
--------------------------------------------------------------------------------

-- vivid blue fg "foo"

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
