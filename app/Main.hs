module Main where

import Control.Monad
import Control.Monad.IO.Class
import Math.SiConverter
import System.Console.Haskeline

exitCommands :: [String]
exitCommands = ["exit", "quit", ":q", ":quit", ":qa"]

repl :: InputT IO ()
repl = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()
    Just input -> do
      unless (elem input exitCommands) $ do
        let result = calculate input
        outputStrLn $ show result
        repl

replSettings :: MonadIO m => Settings m
replSettings = Settings {
    complete = noCompletion,
    historyFile = Nothing,
    autoAddHistory = True
  }

main :: IO ()
main = runInputT replSettings repl
