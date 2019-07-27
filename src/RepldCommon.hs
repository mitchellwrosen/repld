{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepldCommon where

import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed)
import Network.Socket
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>))


getRepldSocketPath :: MonadIO m => m FilePath
getRepldSocketPath =
  liftIO do
    repldDir :: FilePath <-
      lookupEnv "XDG_RUNTIME_DIR" >>= \case
        Nothing ->
          getXdgDirectory XdgData "repld"

        Just dir ->
          pure (dir </> "repld")

    createDirectoryIfMissing True repldDir

    pure (repldDir </> "repld")

managedUnixSocket :: Managed Socket
managedUnixSocket =
  managed
    (bracket
      (socket AF_UNIX Stream defaultProtocol)
      close)
