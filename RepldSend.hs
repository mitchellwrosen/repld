module Main where

import Control.Exception.Safe (catchAny)
import qualified Data.ByteString as ByteString
import Network.Socket
import Network.Socket.ByteString (sendAll)
import RepldCommon
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO

main :: IO ()
main = do
  socketPath <- getRepldSocketPath
  withUnixSocket \sock -> do
    connect sock (SockAddrUnix socketPath) `catchAny` \_ -> exitWith (ExitFailure 1)
    bytes <- ByteString.hGetContents stdin
    sendAll sock bytes
