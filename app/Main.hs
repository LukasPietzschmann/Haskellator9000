module Main where

import System.IO (hFlush, stdout)


main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  -- TODO: Call calculation wrapper here
  putStrLn input
  main
  return ()
