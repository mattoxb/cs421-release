--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

--- Metadata for autograder
--- -----------------------
tag1 = 14356
tag2 = 51836
tag3 = 49947

--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons = undefined

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list = undefined

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval = undefined

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' = undefined

--- ### BinTree

-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree = undefined

--- ### SimpVal

-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp = undefined
