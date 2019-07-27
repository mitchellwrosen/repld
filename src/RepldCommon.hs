module RepldCommon where

import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed)
import Network.Socket
import System.Directory (XdgDirectory(..), getXdgDirectory)


getRepldSocketPath :: MonadIO m => m FilePath
getRepldSocketPath =
  liftIO (getXdgDirectory XdgData "repld.sock")

managedUnixSocket :: Managed Socket
managedUnixSocket =
  managed
    (bracket
      (socket AF_UNIX Stream defaultProtocol)
      close)
