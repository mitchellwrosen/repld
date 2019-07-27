{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import RepldCommon

import Control.Exception.Safe (catchAny)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Data.ByteString (ByteString)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO

import qualified Data.ByteString as ByteString


main :: IO ()
main =
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
