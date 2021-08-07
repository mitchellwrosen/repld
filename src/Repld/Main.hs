module Repld.Main
  ( repld,
    repldSend,
  )
where

import Control.Concurrent.STM
import Control.Exception.Safe (catchAny, throwIO)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Ki
import Repld.Ansi
import Repld.Prelude
import Repld.Repl
import Repld.Server (runServer)
import qualified Repld.Socket as Socket
import qualified System.Console.ANSI as Console
import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Terminal.Size as Console
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO

--------------------------------------------------------------------------------
-- repld
-------------------------------------------------------------------------------

repld :: IO ()
repld = do
  command <-
    getArgs >>= \case
      [command] -> pure command
      _ -> exitFailure

  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  socketPath <- getRepldSocketPath
  whenM (doesFileExist socketPath) do
    hPutStrLn stderr ("The repld socket " ++ socketPath ++ " already exists.")
    hPutStrLn stderr "Perhaps repld is already running, or crashed?"
    exitWith (ExitFailure 1)

  runRepl command \repl -> do
    Ki.scoped \scope -> do
      -- Handle client requests by forwarding all lines to the repl.
      Ki.fork_ scope do
        runServer socketPath \case
          Socket.Frame "hello" _ -> pure (Socket.Frame "hello" "")
          Socket.Frame "send" bytes -> do
            handleClientInput repl bytes
            pure (Socket.Frame "send" "")
          _ -> pure (Socket.Frame "error" "unrecognized frame")
      -- Forward our stdin into the repl. This makes the foregrounded repld server interactive.
      Ki.fork_ scope do
        let loop :: Haskeline.InputT IO ()
            loop =
              whenJustM (Haskeline.getInputLine "") \line -> do
                liftIO (writelnRepl repl (Text.pack line))
                loop
        Haskeline.runInputT Haskeline.defaultSettings loop
        closeReplStdin repl
      -- Forward repl's stdout to our stdout.
      Ki.fork_ scope (forever (readRepl repl >>= Text.putStr))
      exitCode <- atomically (waitForReplToExit repl)
      exitWith exitCode

handleClientInput :: Repl -> Text -> IO ()
handleClientInput repl (canonicalizeClientInput -> input) = do
  clearTerminal
  width <- getConsoleWidth
  putReplCommand width
  writeRepl repl (Text.unlines input)
  where
    clearTerminal :: IO ()
    clearTerminal = do
      Console.setCursorPosition 0 0
      Console.clearFromCursorToScreenEnd

    getConsoleWidth :: IO Int
    getConsoleWidth =
      Console.size >>= \case
        Nothing -> throwIO (userError "Failed to get console width")
        Just (Console.Window _ w) -> pure w

    putReplCommand :: Int -> IO ()
    putReplCommand width =
      (putStrLn . style [vivid white bg, vivid black fg])
        (replCommand repl ++ replicate (width - length (replCommand repl)) ' ')

-- | Canonicalize client input by:
--
--   * Removing leading whitespace (but retaining alignment)
--   * Removing trailing whitespace
canonicalizeClientInput :: Text -> [Text]
canonicalizeClientInput text = do
  block <- splitOn [""] (Text.lines text)

  let leadingWhitespace :: Int
      leadingWhitespace =
        -- minimum is safe here (for now...) because if block is null, we don't
        -- force this value
        minimum (map (Text.length . Text.takeWhile isSpace) block)

  let strip :: Text -> Text
      strip =
        Text.stripEnd . Text.drop leadingWhitespace

  filter (not . Text.null) (map strip block)

--------------------------------------------------------------------------------
-- repld-send
-------------------------------------------------------------------------------

repldSend :: IO ()
repldSend = do
  socketPath <- getRepldSocketPath
  (`catchAny` \_ -> exitFailure) do
    Socket.connect socketPath \sock -> do
      text <- Text.hGetContents stdin
      Socket.send sock (Socket.Frame "send" text)
      _ <- Socket.recv sock
      pure ()

getRepldSocketPath :: IO FilePath
getRepldSocketPath = do
  repldDir <-
    lookupEnv "XDG_RUNTIME_DIR" >>= \case
      Nothing -> getXdgDirectory XdgData "repld"
      Just dir -> pure (dir </> "repld")
  createDirectoryIfMissing True repldDir
  pure (repldDir </> "repld")
