{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

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
import Data.Foldable (fold, for_)
import Data.Function ((&), fix)
import Data.Text (Text)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (XdgDirectory(..), getXdgDirectory, removeFile)
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
      Opt.hsubparser
        (fold
          [ Opt.command
            "send"
            (Opt.info
              (pure sendMain)
              (Opt.progDesc "Send"))

          , Opt.command
            "serve"
            (Opt.info
              (serveMain
                <$> Opt.strArgument (Opt.metavar "COMMAND")
                <*> Opt.switch
                      (Opt.long "echo" <>
                       Opt.help "Echo the input sent to the server"))
              (Opt.progDesc "Serve COMMAND"))
          ])

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

--------------------------------------------------------------------------------
-- Serve
--------------------------------------------------------------------------------

serveMain :: String -> Bool -> IO ()
serveMain replCommand echo =
  runManaged do
    disableBuffering stdout
    disableBuffering stderr

    socketPath :: FilePath <-
      getRepldSocketPath

    sock :: Socket <-
      managedUnixSocketServer socketPath

    replProcess :: Process Handle () () <-
      managedProcess replProcessConfig

    doServeMain echo sock replProcess

    stopProcess replProcess

  where
    replProcessConfig :: ProcessConfig Handle () ()
    replProcessConfig =
      shell replCommand
        & setDelegateCtlc True
        & setStdin createPipe

doServeMain
  :: Bool
  -> Socket
  -> Process Handle () ()
  -> Managed ()
doServeMain echo sock replProcess = do
  disableBuffering replProcessStdin

  acceptAsync :: Async () <-
    managedAsync (acceptThread (accept sock) handleClientInput)

  stdinAsync :: Async () <-
    managedAsync (stdinThread handleLocalInput)

  (liftIO . atomically)
    (waitEitherSTM_ acceptAsync stdinAsync <|>
      void (waitExitCodeSTM replProcess))

  where
    handleClientInput :: [Text] -> IO ()
    handleClientInput input = do
      Console.setCursorPosition 0 0
      Console.clearFromCursorToScreenEnd

      when echo do
        for_ input \line ->
          Text.putStrLn
            (fold
              [ Text.pack
                  (Console.setSGRCode
                    [Console.SetColor
                      Console.Foreground
                      Console.Vivid
                      Console.Blue])
              , "≫ "
              , Text.pack (Console.setSGRCode [Console.Reset])
              , line
              ])

        Console.size >>= \case
          Just (Console.Window _ width) ->
            Text.putStrLn (Text.replicate width "─")
          _ ->
            pure ()

      Text.hPutStr replProcessStdin (Text.unlines input)

    handleLocalInput :: Text -> IO ()
    handleLocalInput =
      Text.hPutStrLn replProcessStdin

    replProcessStdin :: Handle
    replProcessStdin =
      getStdin replProcess

-- | Accept clients forever.
acceptThread
  :: IO (Socket, SockAddr)
  -> ([Text] -> IO ())
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
  -> ([Text] -> IO ())
  -> IO ()
clientThread sock handleInput =
  fix \loop -> do
    bytes :: ByteString <-
      recv sock 8192

    unless (ByteString.null bytes) do
      text :: Text <-
        either throwIO pure (Text.decodeUtf8' bytes)
      handleInput (canonicalize text)
      loop

  where
    canonicalize :: Text -> [Text]
    canonicalize text = do
      line <- Text.strip <$> Text.lines text
      guard (not (Text.null line))
      pure line


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
