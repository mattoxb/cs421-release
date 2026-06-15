-- | The parser goes here

module Parse where

import Prelude hiding (id)

import Types

import qualified Data.HashMap.Lazy as M

import Data.Functor.Identity
import Control.Monad
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

id :: Parser Name
id = do first <- oneOf ['a' .. 'z']
        rest <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")
        spaces
        return $ first:rest


  
eInt :: Parser Expr
eInt = do i <- int
          return $ ENum i
          
expr :: Parser Expr
expr = eInt

decl :: Parser Decl
decl = do name <- id
          params <- many id
          _ <- symbol "="
          body <- expr
          return (name,params,body)

core :: Parser Core
core = do decls <- decl `sepBy` (symbol ";")
          return $ M.fromList [(n,v) | v@(n,_,_) <- decls]

parseCore :: String -> Either ParseError Core
parseCore text = parse core "Core" text
