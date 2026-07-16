{-# LANGUAGE FlexibleContexts #-}
module Scheme.Parse where

import Scheme.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

type Parser = ParsecT String () Identity

--- ### Lexers

parseWith :: Parser a -> String -> Either ParseError a
parseWith p = parse p ""

digitP :: Parser Char
digitP = oneOf ['0'..'9']

digitsP :: Parser String
digitsP = many1 digitP

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

idP :: Parser String
idP = liftM2 (:) identFirst (many identRest)
  where identFirst = oneOf $ "-*+/:?><=!" ++ ['a'..'z'] ++ ['A'..'Z']
        identRest  = identFirst <|> digitP

--- ### Value parsers

symP :: Parser Val
symP = Symbol <$> idP

pairList :: [Val] -> Val
pairList [] = Nil
pairList (x:xs) = Pair x (pairList xs)

pairUnfold :: [Val] -> Val -> Val
pairUnfold [] tail = tail
pairUnfold (x:xs) tail = Pair x (pairUnfold xs tail)

-- Parses list and dotted list
listRestP :: Parser Val
listRestP = do exprs <- rawExprP `sepEndBy` maybeSpaceP
               maybeTail <- optionMaybe $ char '.' >> spaceP >> rawExprP
               return $ case maybeTail of
                 Just tail -> pairUnfold exprs tail
                 Nothing -> pairUnfold exprs Nil

-- Parses lists
listP :: Parser Val
listP = do char '(' >> maybeSpaceP
           result <- listRestP
           maybeSpaceP >> char ')'
           return result

numP :: Parser Val
numP = Number . read <$> digitsP

boolP :: Parser Val
boolP = char '#' >> Boolean <$> boolLitP
  where boolLitP = const True <$> char 't'
               <|> const False <$> char 'f'
               <?> "a boolean (#f or #t)"


quoteP :: Parser Val
quoteP = char '\'' >> (\x -> pairUnfold [Symbol "quote", x] Nil) <$> rawExprP

quasiquoteP :: Parser Val
quasiquoteP = char '`' >> (\x -> pairUnfold [Symbol "quasiquote", x] Nil) <$> rawExprP

unquoteP :: Parser Val
unquoteP = char ',' >> (\x -> pairUnfold [Symbol "unquote", x] Nil) <$> rawExprP

rawExprP :: Parser Val
rawExprP = numP
       <|> symP
       <|> boolP
       <|> quoteP
       <|> quasiquoteP
       <|> unquoteP
       <|> listP
       <?> "a value"

exprP :: Parser Val
exprP = between maybeSpaceP maybeSpaceP rawExprP <* eof
