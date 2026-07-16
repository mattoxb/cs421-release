{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp f = PrimFunc p where
  p [Number x, Number y] = return $ Number $ f x y
  p v = throwError $ UnexpectedArgs v

liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
  p [Number x] = return $ Number $ f x
  p v = throwError $ UnexpectedArgs v

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean False] = return $ Boolean $ f False
  p [_] = return $ Boolean $ f True
  p v = throwError $ UnexpectedArgs v

liftCompOp :: (Int -> Int -> Bool) -> Val
liftCompOp f = PrimFunc p where
  p [] = return $ Boolean True
  p xx = mapM lowerInt xx >>= \nums ->
    return . Boolean . and . map (uncurry f) $ zip nums (tail nums)

--- ### Primtive operations

-- Primitive function `car`
car :: [Val] -> EvalState Val
car [Pair x y] = return x
car v = throwError $ UnexpectedArgs v

-- Primitive function `cdr`
cdr :: [Val] -> EvalState Val
cdr [Pair x y] = return y
cdr vv = throwError $ UnexpectedArgs vv

-- Primitive function `cons`
cons :: [Val] -> EvalState Val
cons [x, y] = return $ Pair x y
cons vv = throwError $ UnexpectedArgs vv

-- Primitive function `list`
list :: [Val] -> EvalState Val
list [] = return $ Nil
list (x:xs) =
  do xx <- list xs
     return $ Pair x xx

-- Primitive function `append`
append :: [Val] -> EvalState Val
append [Nil, l2] = return l2
append [Pair x l, l2] =
  do xx <- append [l, l2]
     return $ Pair x xx
append [v] = throwError $ TypeError v
append vv = throwError $ UnexpectedArgs vv

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6
evalPrim :: [Val] -> EvalState Val
evalPrim = const $ unimplemented "Primitive function `eval`"

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
equalSign :: [Val] -> EvalState Val
equalSign [] = return $ Boolean True
equalSign [x] = return $ Boolean True
equalSign l@(x:xs) = equalSignTypeValid l >> equalSignVal l
equalSignTypeValid :: [Val] -> EvalState Val
equalSignTypeValid [] = return $ Boolean True
equalSignTypeValid [x] = return $ Boolean True
equalSignTypeValid l@(x:xs) =
  let same_type (a,b) = case (a,b) of
        ((Number _),(Number _)) -> return $ Boolean True
        ((Boolean _),(Boolean _)) -> return $ Boolean True
        (x,y) -> throwError $ TypeError y
      pairs = zip l (tail l)
      check_pairs [] = return $ Boolean True
      check_pairs ((a,b):xs) = same_type (a,b) >> check_pairs xs
  in check_pairs pairs
equalSignVal :: [Val] -> EvalState Val
equalSignVal [] = return $ Boolean True
equalSignVal [x] = return $ Boolean True
equalSignVal (x:xs) = Boolean <$> foldlM (equal' x) True xs where
  equal' _ False _ = return False
  equal' (Number a) _ (Number b) = return $ a == b
  equal' (Boolean a) _ (Boolean b) = return $ a == b
  equal' y _ _ = throwError $ TypeError y

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
eq :: [Val] -> EvalState Val
eq [] = return $ Boolean True
eq (x:xs) = return $ Boolean $ foldl (eq' x) True xs where
  eq' _ False _ = False
  eq' (Number a) _ (Number b) = a == b
  eq' (Boolean a) _ (Boolean b) = a == b
  eq' (Symbol a) _ (Symbol b) = a == b
  eq' _ _ _ = False

-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
-- TODO
isList :: [Val] -> EvalState Val
isList = const $ unimplemented "Primitive function `list?`"

-- Primitive function `symbol?` predicate
isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol x] = return $ Boolean True
isSymbol [_] = return $ Boolean False
isSymbol e =  throwError $ UnexpectedArgs e

-- Primitive function `pair?` predicate
-- TODO
isPair :: [Val] -> EvalState Val
isPair = const $ unimplemented "Primitive function `pair?`"

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber = const $ unimplemented "Primitive function `number?`"

-- Primitive function `boolean?` predicate
-- TODO
isBoolean :: [Val] -> EvalState Val
isBoolean = const $ unimplemented "Primitive function `boolean?`"

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull :: [Val] -> EvalState Val
isNull = const $ unimplemented "Primitive function `null?`"

--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("*", liftIntVargOp (*) 1)
                     , ("/", liftIntVargOp div 1)
                     , (">", liftCompOp (>))
                     , (">=", liftCompOp (>=))
                     , ("<", liftCompOp (<))
                     , ("<=", liftCompOp (<=))
                     , ("or",  liftBoolVargOp or)
                     , ("and",  liftBoolVargOp and)
                     , ("modulo", liftIntBinOp mod)
                     , ("abs", liftIntUnaryOp abs)
                     , ("not", liftBoolUnaryOp not)
                     , ("car", PrimFunc car)
                     , ("cdr", PrimFunc cdr)
                     , ("cons", PrimFunc cons)
                     , ("=", PrimFunc equalSign)
                     , ("eq?", PrimFunc eq)
                     , ("list", PrimFunc list)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     -- TODO: Insert more runtime bindings here
                     ]
