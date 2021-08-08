module Repld.Server
  ( runServer,
  )
where

import Control.Exception.Safe (tryAny)
import Repld.Prelude
import qualified Repld.Socket as Socket

-- | Run a single-threaded unix socket server, which handles requests from trusted clients one at a time. Each client
-- connects, sends a single request, receives a single response, and closes its socket.
--
-- This function never returns gracefully. It throws an exception if any of the following occur:
--
-- * Binding the server socket throws an exception
-- * Accepting a client socket throws an exception
-- * The server itself (the callback provided to this function) throws an exception when computing the response to a
-- request
--
-- If either receiving a request from or sending a response to a client fails, the exception is ignored.
runServer :: FilePath -> (Socket.Frame -> IO Socket.Frame) -> IO a
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
