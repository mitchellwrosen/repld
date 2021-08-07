module Repld.Server
  ( runServer,
  )
where

import Control.Exception.Safe (tryAny)
import Repld.Prelude
import qualified Repld.Socket as Socket

-- Run a single-threaded unix socket server, which handles requests from trusted clients one at a time. Each client
-- connects, sends a single Text request, waits for a Text response, and closes its socket.
runServer :: FilePath -> (Socket.Frame -> IO Socket.Frame) -> IO ()
runServer path handle =
  Socket.bind path \server ->
    forever do
      Socket.accept server \client ->
        tryAny (Socket.recv client) >>= \case
          Left _ -> pure ()
          Right Nothing -> pure ()
          Right (Just request) -> do
            response <- handle request
            _ <- tryAny (Socket.send client response)
            pure ()
