import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck

import Data.Foldable
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)

import Lib

import UnitTests
import PropertyTests


main :: IO ()    
main = putStrLn "" >> defaultMain tests

tests = testGroup "User Friendly Tests"
  [ unitTests
  , propTests
  ]

buildTestCase :: [(String, Assertion)] -> [TestTree]
buildTestCase = map $ uncurry testCase

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testGroup "Lifting Boolean Binary Functions"
    $ buildTestCase liftBoolOpUnitTests

  , testGroup "Lifting Comparison Binary Functions"
    $ buildTestCase liftCompOpUnitTests
    
  , testGroup "eval Unit Tests"
    [ testGroup "Constant Expressions"
      $ buildTestCase constExpUnitTests
      
    , testGroup "VariableExpressions"
      $ buildTestCase varExpUnitTests

    , testGroup "Operator Expressions"
      [ testGroup "Int Operator Expressions"
        $ buildTestCase intOpExpUnitTests
      , testGroup "Bool Operator Expressions"
        $ buildTestCase boolOpExpUnitTests
      , testGroup "Comp Operator Expressions"
        $ buildTestCase compOpExpUnitTests
      ]
      
    , testGroup "If Expressions"
      $ buildTestCase ifExpUnitTests
      
    , testGroup "Function Expressions"
      $ buildTestCase funExpUnitTests
      
    , testGroup "Application Expressions"
      $ buildTestCase appExpUnitTests
      
    , testGroup "Let Expressions"
      $ buildTestCase letExpUnitTests
    ]
  , testGroup "exec Unit Tests"
    [ testGroup "Set Statements"
      $ buildTestCase setStmtUnitTests
      
    , testGroup "Sequence Statements"
      $ buildTestCase seqStmtUnitTests
    , testGroup "If Statements"
      $ buildTestCase ifStmtUnitTests
 
    , testGroup "Procedure Statements"
      $ buildTestCase procStmtUnitTests
      
    , testGroup "Procedure Call Statements"
      $ buildTestCase callStmtUnitTests
    ]
  ]

propTests :: TestTree
propTests = testGroup "Property Tests"
  [ testGroup "Lift Functions"
    [ testProperty
      "Lifts Boolean Operations"
      $ forAll arbLiftBool $ liftOp_prop liftBoolOp
      
    , testProperty
      "Lifts Comparison Operations"
      $ forAll arbLiftComp $ liftOp_prop liftCompOp
    ]
    
  , testGroup "eval Function"
    [ testProperty
      "Constant Expressions"
      $ forAll arbConstExp anyExpVal_prop
      
    , testProperty
      "Variable Expressions"
      $ forAll arbVarExp anyExpEnvVal_prop
      
    , testProperty
      "Integer Operation Expressions"
      $ forAll (arbIntOpExp 3)  anyExpVal_prop
      
    , testProperty
      "Comparison Operation Expressions"
      $ forAll (arbCompOpExp 3) anyExpVal_prop
      
    , testProperty
      "Boolean Operation Expressions"
      $ forAll (arbBoolOpExp 3) anyExpVal_prop
      
    , testProperty
      "If Expressions"
      $ forAll arbIfExp anyExpVal_prop
      
    , testProperty
      "Function Expressions"
      $ forAll arbFunExp anyExpEnvVal_prop
      
    , testProperty
      "Function Application"
      $ forAll arbAppExp anyExpEnvVal_prop
      
    , testProperty
      "Let Expressions"
      $ forAll arbLetExp anyExpEnvVal_prop
    ]

  , testGroup "exec Function"
    [ testProperty
      "Assign Statements"
      $ forAll arbSetStmt anyStmtEnvPEnvVal_prop
      
    , testProperty
      "Sequence Statements"
      $ forAll arbSeqStmt anyStmtEnvPEnvVal_prop
    , testProperty
      "If Statements"
      $ forAll arbIfStmt anyStmtEnvPEnvVal_prop
      
    , testProperty
      "Procedure Statements"
      $ forAll arbProcStmt anyStmtEnvPEnvVal_prop
      
    , testProperty
      "Call Procedure Statements"
      $ forAll arbCallStmt anyStmtEnvPEnvVal_prop
    ]
  ]
