--- Given Code
--- ==========

module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Lib
import Parse

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parseDecl input of
                            Left err    -> do printLn "Parse error!"
                                              printLn $ show err
                            Right decl  -> printLn . show $ cpsDecl decl
                         repl


main :: IO ()
main = do putStrLn "Welcome to the CPS Transformer!"
          repl
          putStrLn "GoodBye!"
