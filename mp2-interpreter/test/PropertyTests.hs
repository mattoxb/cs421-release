module PropertyTests
  ( module ExpGens
  , module StmtGens
  , liftOp_prop
  , anyExpVal_prop
  , anyExpEnvVal_prop
  , anyStmtEnvPEnvVal_prop
  )
where

import Test.QuickCheck
import qualified Data.HashMap.Strict as H

import Lib

import ExpGens
import StmtGens

isException :: Val -> Bool
isException (ExnVal _) = True
isException         _  = False

--- ### Exp Properties
liftOp_prop
  :: Show lit
  => ((lit -> lit -> Bool) -> Val -> Val -> Val) -- ^ Lifting Function 
  -> LiftOpUnit lit -- ^ Generated Unit test 
  -> Property
liftOp_prop liftFun LiftOpUnit
    { arg1, arg2, op, expResult }
    = classify (isException expResult) "Exception"
      $ showFailure $ actualResult === expResult
  where
    actualResult = liftFun (applyFun2 op) arg1 arg2
    showFailure = counterexample $ "liftOp failed on \noperator: '"++show op
                  ++"' called on\nargument1: '"++show arg1
                  ++"' and\nargument2: '"++show arg2
                  ++"'.\nExpected: '" ++ show expResult
                  ++"'\nbut got: '" ++ show actualResult ++"'"
                          
anyExpVal_prop :: ExpValUnit -> Property
anyExpVal_prop (ExpValUnit e v)
    = classify (isException v) "Exception"
      $ showFailure $ actualResult === v
  where
    actualResult = eval e H.empty
    showFailure = counterexample $ "eval failed on"
                  ++"\nexpression: '"++show e
                  ++"'.\nExpected: '"++show v
                  ++"'\nbut got: '"++show actualResult++"'"

anyExpEnvVal_prop :: ExpEnvValUnit -> Property
anyExpEnvVal_prop (ExpEnvValUnit e env v)
    = classify (isException v) "Exception"
      $ showFailure $ actualResult === v
  where
    actualResult = eval e env
    showFailure = counterexample $ "eval failed on"
                  ++"\nexpression: '"++show e
                  ++"'\nin environment: '"++show env
                  ++"'.\nExpected: '"++show v
                  ++"'\nbut got: '"++show actualResult++"'"

--- ### Stmt Properties
anyStmtEnvPEnvVal_prop :: StmtEnvPEnvResultUnit -> Property
anyStmtEnvPEnvVal_prop StmtEnvPEnvResultUnit
    { stmt, env, penv, stmtResult } = showFailure $ actualResult === stmtResult
  where
    actualResult = exec stmt penv env
    showFailure = counterexample $ "exec failed on"
                  ++"\nstatement: '"++show stmt
                  ++"'\nin environment: '"++show env
                  ++"'\nand procedure environment: '"++ show penv
                  ++"'.\nExpected: '"++show stmtResult
                  ++"'\nbut got: '"++show actualResult++"'"

  
