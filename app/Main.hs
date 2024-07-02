module Main (main) where

import Control.Monad
import Control.Monad.IO.Class

import Math.SiConverter

import System.Console.Haskeline
import System.Directory

exitCommands :: [String]
exitCommands = ["exit", "quit", ":q", ":quit", ":qa"]

repl :: InputT IO ()
repl = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just input -> do
            unless (input `elem` exitCommands) $ do
                case calculate input of
                    Right res -> outputStrLn $ "= " ++ show res
                    Left err  -> outputStrLn $ show err
                repl

replSettings :: MonadIO m => IO (Settings m)
replSettings = do
    stateDir <- getXdgDirectory XdgState ""
    doesDirExist <- doesDirectoryExist stateDir
    histFilePath <- if doesDirExist
        then do
            let filepath = stateDir ++ "/haskellator-history"
            putStrLn $ "Saving REPL history to " ++ filepath
            return $ pure filepath
        else putStrLn "Could not find temp dir. REPL history will not be saved." >> return Nothing
    return $ Settings {
        complete = noCompletion,
        historyFile = histFilePath,
        autoAddHistory = True
    }

main :: IO ()
main = replSettings >>= flip runInputT repl
