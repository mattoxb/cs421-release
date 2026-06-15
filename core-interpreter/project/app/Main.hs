--- Given Executable Code
--- =====================

module Main where

--- Initial State
--- -------------

import System.IO (hFlush, stdout)
import Parse
import Interp
import Types

rep :: IO ()
rep = do
  text <- getContents                                 -- Read
  case parseCore text of                              -- Parse
    Left err -> print err                             -- Diagnostics
    Right program ->
      print $ run program   -- Eval

repl :: Env -> IO ()
repl = undefined -- Implement this is you want a REPL instead of a batch processor


main = do putStrLn "Welcome to your Core interpreter!"
          putStrLn ""
          putStrLn "Enter your program here and hit ^D when done."
          rep 

