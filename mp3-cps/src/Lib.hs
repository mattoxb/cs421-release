--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) $ k . (*n)

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] keven kodd
    | even x = keven x
    | otherwise = kodd x
evenoddk (x:xs) keven kodd
    | even x = 
        evenoddk xs
            (\sumE -> keven (x + sumE))
            kodd
    | otherwise = 
        evenoddk xs
            keven  
                (\sum0 -> kodd (x + sum0))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (OpExp _ e1 e2) =
  isSimple e1 && isSimple e2
isSimple (IfExp c t e) = 
    isSimple c && isSimple t && isSimple e
isSimple (AppExp _ _) = False
isSimple (LamExp _ _) = True

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n =
  (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n =
  (AppExp k (VarExp v), n)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f a) k n 
    | isSimple a = 
        (AppExp (AppExp f a) k, n)
    | otherwise = 
        let (v, n1) = gensym n
            cont = LamExp v (AppExp (AppExp f (VarExp v))k)
        in cpsExp a cont n1
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n
  -- Case 1: both simple
  | isSimple e1 && isSimple e2 =
      (AppExp k (OpExp op e1 e2), n)
  | not (isSimple e1) && isSimple e2 =
      let (v1, n1) = gensym n
          cont1    = LamExp v1 (AppExp k (OpExp op (VarExp v1) e2))
      in cpsExp e1 cont1 n1
  | isSimple e1 && not (isSimple e2) =
      let (v2, n1) = gensym n
          cont2    = LamExp v2 (AppExp k (OpExp op e1 (VarExp v2)))
      in cpsExp e2 cont2 n1
  | otherwise =
      let (v1, n1) = gensym n
          (v2, n2) = gensym n1
          cont2 = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
          (e2', n3) = cpsExp e2 cont2 n2
          cont1 = LamExp v1 e2'
      in cpsExp e1 cont1 n3
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n
  | isSimple e1 =
      let (e2', n1) = cpsExp e2 k n
          (e3', n2) = cpsExp e3 k n1
      in (IfExp e1 e2' e3', n2)
  | otherwise =
      let (v, n1) = gensym n
          (e2', n2) = cpsExp e2 k n1
          (e3', n3) = cpsExp e3 k n2
          cont = LamExp v (IfExp (VarExp v) e2' e3')
      in cpsExp e1 cont n3

--- ### Define `cpsDecl`
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f args body) = 
    let k = "k"
        (body', _) = cpsExp body (VarExp k) 0
    in Decl f (args ++ [k]) body'
