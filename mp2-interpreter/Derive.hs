module Derive where

import System.Environment (getArgs)
import Data.List (intercalate)

import Text.Pandoc (Pandoc, Block(CodeBlock, Null), def, writeMarkdown, writerColumns, readMarkdown)
import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Error (handleError)

main :: IO ()
main = do (fileName:_) <- getArgs
          putStrLn $ "Input file: " ++ fileName
          fileContents <- readFile fileName
          let doc = handleError . readMarkdown def $ fileContents
          makeFile "README.md" doc
          makeFile "app/Main.hs" doc
          makeFile "soln/Solution.hs" doc

makeFile :: String -> Pandoc -> IO ()
makeFile name doc = do putStrLn $ "Writing file: " ++ name
                       writeFile name $ fileGen name doc

fileGen :: String -> Pandoc -> String
fileGen "README.md"        = writeREADME . walk (keepCodeBlock $ tag "example")
fileGen "app/Main.hs"      = writeCode   . walk (keepCodeBlock $ allOf [tag "haskell", tag "given"])
fileGen "soln/Solution.hs" = writeCode   . walk (keepCodeBlock $ allOf [tag "haskell", tag "solution"])
fileGen name               = error $ "File '" ++ name ++ "' not recognized."

writeREADME :: Pandoc -> String
writeREADME = writeMarkdown (def {writerColumns = 80}) . walk sanitizeCode

writeCode :: Pandoc -> String
writeCode = intercalate "\n\n" . map (\(CodeBlock _ code) -> code) . query codeBlock

keepCodeBlock :: ([String] -> Bool) -> Block -> Block
keepCodeBlock p cb@(CodeBlock (_, cs, _) _) = if p cs then cb else Null
keepCodeBlock _ b                           = b

sanitizeCode :: Block -> Block
sanitizeCode (CodeBlock (_, cs, _) code)
    | "haskell" `elem` cs = CodeBlock ("", ["haskell"], []) code
    | "sh" `elem` cs      = CodeBlock ("", ["sh"], [])      code
    | otherwise           = CodeBlock ("", [], [])          code
sanitizeCode b            = b

codeBlock :: Block -> [Block]
codeBlock cb@(CodeBlock _ _) = [cb]
codeBlock _                  = []

tag :: String -> [String] -> Bool
tag = elem

anyOf :: [([String] -> Bool)] -> [String] -> Bool
anyOf ps str = or $ map ($ str) ps

allOf :: [([String] -> Bool)] -> [String] -> Bool
allOf ps str = and $ map ($ str) ps
