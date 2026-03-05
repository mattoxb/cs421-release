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
factk n k =
    if n == 0 then k 1
    else factk (n-1) (\v -> k $ v * n)
-- would be called like factk 5 (\v -> factk 5-1)... I think

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x]    k1 k2   =
    -- even case
    if (x `mod` 2) == 0 then k1 x -- feed into the even functions the sum of even numbers
    else                     k2 x -- and vice versa with the odd functions

-- similarly, we have the following for when recursing down the tree
evenoddk (x:xs) k1 k2 =
    if (x `mod` 2) == 0 
        then evenoddk xs (\v -> k1 $ x + v) k2
    else evenoddk xs k1 (\v -> k2 $ x + v)


--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

-- From my understanding, it seems like we only need to recurse if we ever see an instance of an AppExp. 
-- If we do, we return False, o/w return True

isSimple :: Exp -> Bool
isSimple (AppExp _ _) = False -- DoesnPrimet matter what comes next - this is false
isSimple (OpExp _ y z) = isSimple y && isSimple z -- Doesn't matter what comes next - this is false
isSimple (IfExp x y z) = isSimple x && isSimple y && isSimple z -- Doesn't matter what comes next - this is false
-- o/w, we reach a case were the following will always return a True / their own values in regular expressions.
isSimple (IntExp _) = True 
isSimple (VarExp _) = True 

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
-- These cases are simple enough
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)

cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n =
    if isSimple e then (AppExp (AppExp f e) k, n)
    else 
        let
            (v1, n1) = gensym n
            exprsn = AppExp (AppExp f (VarExp v1)) k
            lmda = LamExp v1 exprsn -- wrap the expression to be evaluated in the AST in the lambda, then process e
        in cpsExp e lmda n1

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n =
    if isSimple e1 && isSimple e2 then (AppExp k (OpExp op e1 e2), n)
    else if isSimple e2 then
        let
            (v1, n1) = gensym n
            exprsn = AppExp k (OpExp op (VarExp v1) e2)
            lmda = LamExp v1 exprsn
        in cpsExp e1 lmda n1
    else if isSimple e1 then
        let
            (v1, n1) = gensym n
            exprsn = AppExp k (OpExp op e1 (VarExp v1))
            lmda = LamExp v1 exprsn
        in cpsExp e2 lmda n1
    else
        let
            (v1, n1) = gensym n
            (v2, n2) = gensym n1
            lmda1 = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
            (innerE, n3) = cpsExp e2 lmda1 n2
            lmda2 = LamExp v1 innerE
        in cpsExp e1 lmda2 n3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n =
    if isSimple e1 then
        let
            -- We want to call cpsExp on both e2 and e3 in order to get
            -- the AST representation of what they would be in our nesting.
            (e2Prime, n1) = cpsExp e2 k n
            (e3Prime, n2) = cpsExp e3 k n1
        in (IfExp e1 e2Prime e3Prime, n2)
    else
        let
            (v1, n1) = gensym n
            -- Ditto here
            (e2Prime, n2) = cpsExp e2 k n1
            (e3Prime, n3) = cpsExp e3 k n2
            exprsn = (IfExp (VarExp v1) e2Prime e3Prime)
            lmda = LamExp v1 exprsn
        in cpsExp e1 lmda n1

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl stm ss e) =
    let
        ss1 = ss ++ ["k"] -- new parameter: the continuation function
        (trnsfrmed, _) = cpsExp e (VarExp "k") 0 -- now we treat that "k"ontinuation as the first thing to continue when being passed the items from the original bodys evaluation
    in (Decl stm ss1 trnsfrmed)
