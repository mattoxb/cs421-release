-- This file shows how to implement a recursive let
-- It's not part of the project.

import qualified Data.Map as M

data Exp = EInt Int
         | EPlus Exp Exp
         | ELetRec [(String,Exp)] Exp
         | EVar String
    deriving (Eq,Show)

data Val = VInt Int
    deriving (Eq,Show)


type Env = M.Map String Val

eval :: Exp -> Env -> Val
eval (EVar x) env =
  case M.lookup x env of
    Nothing -> error "It broke."
    Just z -> z

eval (EInt i) env = VInt i
eval (EPlus e1 e2) env =
  let VInt v1 = eval e1 env
      VInt v2 = eval e2 env
   in VInt $ v1 + v2

eval (ELetRec [] body) env = eval body env
eval (ELetRec bindings body) env =
  let env' = M.union defs env
      defs = M.fromList [(name,eval exp env')  | (name,exp) <- bindings]
   in eval body env'

initial :: Env
initial = M.insert "z" (VInt 7) M.empty

foo = let exp = ELetRec [("x", EPlus (EVar "y") (EInt 10))
                        ,("y", EPlus (EVar "z") (EInt 5))]
                (EPlus (EVar "x") (EVar "y"))
       in eval exp initial
