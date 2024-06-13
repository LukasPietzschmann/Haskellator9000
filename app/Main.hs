module Main where

import System.IO (hFlush, stdout)
import Math.SiConverter


main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let result = calculate input
  print result
  main
  return ()
