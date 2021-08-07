-- | A small unix socket API with (TODO) framing.
module Repld.Socket
  ( Frame,
    Socket,
    accept,
    bind,
    connect,
    Socket.listen,
    new,
    recv,
    send,
  )
where

import Control.Exception.Safe (bracket, bracket_, catchAny, throwIO)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.Socket (Socket)
import qualified Network.Socket as Socket
  ( Family (AF_UNIX),
    SockAddr (..),
    SocketType (Stream),
    accept,
    bind,
    close,
    connect,
    defaultProtocol,
    listen,
    socket,
  )
import qualified Network.Socket.ByteString as Socket (recv, sendAll)
import System.Directory (removeFile)

-- TODO framing
type Frame =
  Text

accept :: Socket -> IO Socket
accept =
  fmap fst . Socket.accept

bind :: FilePath -> (Socket -> IO a) -> IO a
bind path action =
  new \socket ->
    bracket_ (Socket.bind socket (Socket.SockAddrUnix path)) (ignoringExceptions (removeFile path)) (action socket)

connect :: FilePath -> (Socket -> IO a) -> IO a
connect path action =
  new \socket -> do
    Socket.connect socket (Socket.SockAddrUnix path)
    action socket

new :: (Socket -> IO a) -> IO a
new =
  bracket (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) Socket.close

-- | Receive a single frame, or Nothing if the peer has disconnected.
recv :: Socket -> IO (Maybe Frame)
recv socket = do
  -- TODO framing; here we just assume a frame is read in one up-to-8k read
  bytes <- Socket.recv socket 8192
  if ByteString.null bytes
    then pure Nothing
    else case Text.decodeUtf8' bytes of
      Left ex -> throwIO ex
      Right text -> pure (Just text)

-- | Send a single frame.
send :: Socket -> Frame -> IO ()
send socket text =
  Socket.sendAll socket (Text.encodeUtf8 text)

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()
