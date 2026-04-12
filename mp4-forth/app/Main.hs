--- Given Executable Code
--- =====================

module Main where

--- Initial State
--- -------------

import System.IO (hFlush, stdout)
import Lib (IStack, ForthState, initialDictionary, eval)

-- initial integer stack
initialIStack :: IStack
initialIStack = []

-- initial output
initialOutput :: [String]
initialOutput = []

-- initial ForthState
initialForthState :: ForthState
initialForthState = (initialIStack, initialDictionary, initialOutput)

--- Read-Eval-Print Loop
--- ------------------------

repl :: ForthState -> IO ()
repl state
    = do putStr "> "
         hFlush stdout
         input <- getLine
         if input == "bye"
            then do putStrLn "Bye!"
                    return ()
            else let (is, d, output) = eval (words input) state
                 in  do mapM_ putStrLn (reverse output)
                        repl (is, d, [])

main = do putStrLn "Welcome to your Forth interpreter!"
          repl initialForthState

