module RepldCommon where

import Control.Exception.Safe (bracket)
import Network.Socket
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>))

getRepldSocketPath :: IO FilePath
getRepldSocketPath = do
  repldDir <-
    lookupEnv "XDG_RUNTIME_DIR" >>= \case
      Nothing -> getXdgDirectory XdgData "repld"
      Just dir -> pure (dir </> "repld")
  createDirectoryIfMissing True repldDir
  pure (repldDir </> "repld")

withUnixSocket :: (Socket -> IO a) -> IO a
withUnixSocket =
  bracket
    (socket AF_UNIX Stream defaultProtocol)
    close
