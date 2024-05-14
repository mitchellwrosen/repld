-- | A small unix socket API with (TODO) framing.
module Repld.Socket
  ( Frame (..),
    Socket,
    accept,
    bind,
    connect,
    new,
    recv,
    send,
  )
where

import Control.Exception.Safe (bracket, bracketOnError, bracket_, catchAny)
import Cretheus.Decode qualified
import Cretheus.Encode qualified
import Data.ByteString qualified as ByteString
import Network.Socket (Socket)
import Network.Socket qualified as Socket
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
import Network.Socket.ByteString qualified as Socket (recv, sendAll)
import Repld.Prelude
import System.Directory (removeFile)

accept :: Socket -> (Socket -> IO a) -> IO a
accept server =
  bracketOnError (fst <$> Socket.accept server) Socket.close

bind :: FilePath -> (Socket -> IO a) -> IO a
bind path action =
  new \socket ->
    bracket_ (Socket.bind socket (Socket.SockAddrUnix path)) (ignoringExceptions (removeFile path)) do
      Socket.listen socket 5
      action socket

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
  -- TODO here we just assume a frame is read in one up-to-8k read
  bytes <- Socket.recv socket 8192
  pure
    if ByteString.null bytes
      then Nothing
      else case deserializeFrame bytes of
        Left _ -> Nothing
        Right frame -> Just frame

-- | Send a single frame.
send :: Socket -> Frame -> IO ()
send socket frame =
  Socket.sendAll socket (serializeFrame frame)

-- | Run an IO action, ignoring synchronous exceptions
ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

-- Frame

data Frame
  = Frame Text Text

deserializeFrame :: ByteString -> Either Text Frame
deserializeFrame =
  Cretheus.Decode.fromBytes $
    Cretheus.Decode.object do
      type_ <- Cretheus.Decode.property "type" Cretheus.Decode.text
      data_ <- Cretheus.Decode.property "data" Cretheus.Decode.text
      pure (Frame type_ data_)

serializeFrame :: Frame -> ByteString
serializeFrame (Frame type_ data_) =
  Cretheus.Encode.asBytes $
    Cretheus.Encode.object
      [ Cretheus.Encode.property "type" (Cretheus.Encode.text type_)
      , Cretheus.Encode.property "data" (Cretheus.Encode.text data_)
      ]
