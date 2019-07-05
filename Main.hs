{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, waitEitherSTM_, withAsync)
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket, catchAny, finally)
import Control.Monad (forever, join, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, managed, runManaged)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Function ((&))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI (clearFromCursorToScreenEnd, setCursorPosition)
import System.Directory
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO
import System.Process.Typed

import qualified Data.ByteString as ByteString
import qualified Options.Applicative as Opt


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
                <$> Opt.strArgument (Opt.metavar "COMMAND"))
              (Opt.progDesc "Serve COMMAND"))
          ])


--------------------------------------------------------------------------------
-- Send
--------------------------------------------------------------------------------

sendMain :: IO ()
sendMain = do
  socketPath :: FilePath <-
    getRepldSocketPath

  bytes <- ByteString.hGetContents stdin
  ByteString.hPutStr stdout bytes

  withUnixSocket
    (\sock -> do
      connect sock (SockAddrUnix socketPath) `catchAny` \_ ->
        exitWith (ExitFailure 1)
      sendAll sock bytes)


--------------------------------------------------------------------------------
-- Serve
--------------------------------------------------------------------------------

serveMain :: String -> IO ()
serveMain replCommand = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  socketPath :: FilePath <-
    getRepldSocketPath

  let
    replProcessConfig :: ProcessConfig Handle () ()
    replProcessConfig =
      shell replCommand
        & setDelegateCtlc True
        & setStdin createPipe

  withUnixSocket
    (\sock -> do
      bind sock (SockAddrUnix socketPath)
      listen sock 5
      withProcessWait replProcessConfig \replProcess -> do
        serveMain2 sock replProcess
        stopProcess replProcess)
    `finally` ignoringExceptions (removeFile socketPath)

serveMain2
  :: Socket
  -> Process Handle () ()
  -> IO ()
serveMain2 sock replProcess = do
  hSetBuffering (getStdin replProcess)  NoBuffering

  runManaged do
    acceptAsync <-
      managedAsync
        (acceptThread
          (accept sock)
          (ByteString.hPut (getStdin replProcess)))

    stdinAsync <-
      managedAsync
        (stdinThread
          (ByteString.hPut (getStdin replProcess)))

    (liftIO . atomically)
      (waitEitherSTM_ acceptAsync stdinAsync <|>
        void (waitExitCodeSTM replProcess))

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
      bytes <- recv sock 8192

      unless (ByteString.null bytes) do
        setCursorPosition 0 0
        clearFromCursorToScreenEnd
        handleInput bytes
        loop


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getRepldSocketPath :: IO FilePath
getRepldSocketPath =
  getXdgDirectory XdgData "repld.sock"

withUnixSocket :: (Socket -> IO a) -> IO a
withUnixSocket =
  bracket
    (socket AF_UNIX Stream defaultProtocol)
    close

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

managedAsync :: IO a -> Managed (Async a)
managedAsync action =
  managed (withAsync action)
