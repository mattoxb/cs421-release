module Main where

import Common
import Infer
import Parser

import Data.Map.Strict as H (Map, empty)

step :: TypeEnv -> IO (Maybe TypeEnv)
step env = do
  putStrLn "Enter an expression:"
  line <- getLine
  case line of
    "quit" -> putStrLn "bye!" >> return Nothing
    _ -> Just <$> parseAndInfer env line

  where
    parseAndInfer env line = do
      let d = parse (lexer line)
      putStrLn "Parsed as:"
      putStrLn ("  " ++ show d)
      case runInfer (inferDec env d) of
        Right (env', tau) -> do
          putStrLn "Inferred type:"
          putStrLn ("  " ++ show tau)
          return env'
        Left e -> do
          putStrLn "Error while type inferencing:"
          putStrLn ("  " ++ show e)
          return env

repl :: TypeEnv -> IO ()
repl env = do
  menv' <- step env
  case menv' of
    Nothing -> pure () -- quit
    Just env' -> repl env'

main :: IO ()
main = repl H.empty