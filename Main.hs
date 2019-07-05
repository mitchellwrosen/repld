{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, managed, runManaged)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Function
import Network.Socket
import Network.Socket.ByteString
import System.Directory
import System.Exit
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
-- Serve
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
    replProcessConfig :: ProcessConfig Handle Handle Handle
    replProcessConfig =
      shell replCommand
        & setStdin createPipe
        & setStdout createPipe
        & setStderr createPipe

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
  -> Process Handle Handle Handle
  -> IO ()
serveMain2 sock replProcess = do
  stdinChan <- newTChanIO

  hSetBuffering (getStdin replProcess)  NoBuffering
  hSetBuffering (getStdout replProcess) NoBuffering
  hSetBuffering (getStderr replProcess) NoBuffering

  runManaged do
    acceptAsync <-
      managedAsync
        (acceptThread
          (accept sock)
          (atomically . writeTChan stdinChan))

    stdinAsync <-
      managedAsync
        (stdinThread
          (ByteString.hPut (getStdin replProcess)))

    replStdinAsync <-
      managedAsync
        (replStdinThread
          (atomically (readTChan stdinChan))
          (ByteString.hPut (getStdin replProcess)))

    replStdoutAsync <-
      managedAsync
        (replStdoutThread
          (ByteString.hGetSome (getStdout replProcess) 4096))

    replStderrAsync <-
      managedAsync
        (replStderrThread
          (ByteString.hGetSome (getStderr replProcess) 4096))

    (liftIO . void . waitAny)
      [ acceptAsync
      , stdinAsync
      , replStdinAsync
      , replStdoutAsync
      , replStderrAsync
      ]

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

-- Handle one connected client's socket by forwarding all data received.
clientThread
  :: Socket
  -> (ByteString -> IO ())
  -> IO ()
clientThread sock handleInput =
  loop

  where
    loop :: IO ()
    loop = do
      bytes <- recv sock 4096

      unless (ByteString.null bytes) do
        handleInput bytes
        loop

replStdinThread
  :: IO ByteString -- Action that receives input from a connected client
  -> (ByteString -> IO ()) -- Write to repl process's stdin
  -> IO ()
replStdinThread receiveInput putReplStdin =
  forever do
    bytes <- receiveInput
    ByteString.hPutStr stdout bytes
    putReplStdin bytes

replStdoutThread
  :: IO ByteString -- Action that receives stdout from the repl
  -> IO ()
replStdoutThread getReplStdout =
  loop

  where
    loop :: IO ()
    loop = do
      bytes <- getReplStdout

      unless (ByteString.null bytes) do
        ByteString.hPutStr stdout bytes
        loop

replStderrThread
  :: IO ByteString -- Action that receives stderr from the repl
  -> IO ()
replStderrThread getReplStderr =
  loop

  where
    loop :: IO ()
    loop = do
      bytes <- getReplStderr

      unless (ByteString.null bytes) do
        ByteString.hPutStr stderr bytes
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
