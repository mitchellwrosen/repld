{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, waitEitherSTM_, withAsync)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket, bracket_, catchAny)
import Control.Monad (forever, join, unless, when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed, runManaged)
import Data.ByteString (ByteString)
import Data.Foldable (fold, for_)
import Data.Function ((&))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO
import System.Process.Typed

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative as Opt
import qualified System.Console.ANSI as Console


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
serveMain replCommand echo = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  socketPath :: FilePath <-
    getRepldSocketPath

  withUnixSocket
    (\sock ->
      bracket_
        (bind sock (SockAddrUnix socketPath))
        (ignoringExceptions (removeFile socketPath))
        (do
          listen sock 5
          withProcessWait replProcessConfig \replProcess -> do
            doServeMain echo sock replProcess
            stopProcess replProcess))

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
  -> IO ()
doServeMain echo sock replProcess =
  runManaged do
    liftIO (hSetBuffering (getStdin replProcess) NoBuffering)

    acceptAsync :: Async () <-
      managedAsync (acceptThread (accept sock) handleInput)

    stdinAsync :: Async () <-
      managedAsync (stdinThread (ByteString.hPut (getStdin replProcess)))

    (liftIO . atomically)
      (waitEitherSTM_ acceptAsync stdinAsync <|>
        void (waitExitCodeSTM replProcess))

  where
    handleInput :: ByteString -> IO ()
    handleInput input = do
      Console.setCursorPosition 0 0
      Console.clearFromCursorToScreenEnd

      when echo do
        case Text.decodeUtf8' input of
          Left _ ->
            pure ()

          Right text ->
            for_ (Text.lines text) \line ->
              Text.putStrLn $
                fold
                  [ Text.pack
                      (Console.setSGRCode
                        [Console.SetColor
                          Console.Foreground
                          Console.Vivid
                          Console.Blue])
                  , "≫ "
                  , Text.pack (Console.setSGRCode [Console.Reset])
                  , line
                  ]

      ByteString.hPut (getStdin replProcess) input

-- | Accept clients forever.
acceptThread
  :: IO (Socket, SockAddr)
  -> (ByteString -> IO ())
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
  :: (ByteString -> IO ())
  -> IO ()
stdinThread handleInput =
  ignoringExceptions loop

  where
    loop :: IO ()
    loop =
      forever do
        bytes <- ByteString.hGetLine stdin
        handleInput (bytes <> ByteString.singleton 10)

-- | Handle one connected client's socket by forwarding all data on stdin to
-- the repl.
--
-- Clears the screen before forwarding to the repl, because although the input
-- is on a stream socket, I am assuming that I receive an entire "request" in
-- one 8K read.
clientThread
  :: Socket
  -> (ByteString -> IO ())
  -> IO ()
clientThread sock handleInput =
  loop

  where
    loop :: IO ()
    loop = do
      bytes :: ByteString <-
        recv sock 8192

      unless (ByteString.null bytes) do
        handleInput bytes
        loop


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getRepldSocketPath :: MonadIO m => m FilePath
getRepldSocketPath =
  liftIO (getXdgDirectory XdgData "repld.sock")

withUnixSocket :: (Socket -> IO a) -> IO a
withUnixSocket =
  bracket
    (socket AF_UNIX Stream defaultProtocol)
    close

managedUnixSocket :: Managed Socket
managedUnixSocket =
  managed withUnixSocket

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

managedAsync :: IO a -> Managed (Async a)
managedAsync action =
  managed (withAsync action)
