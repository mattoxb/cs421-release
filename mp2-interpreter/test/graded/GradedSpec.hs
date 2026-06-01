import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck
import Test.QuickCheck.IO

import Data.Foldable
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)

import Lib

import UnitTests
import PropertyTests

preamble =
  "Caution, This output is not meant for mortal eyes.  If you\
  \pass Friendly tests\
  \but do not pass the graded tests, please notify course staff\
  \immediately."


main :: IO ()    
main =  putStrLn preamble
     >> putStrLn ""
     >> defaultMain tests

tests = testGroup "Graded Tests"
  [ testGroup "=G= Lift Functions"
    [ testProperty "=P= Lifts Boolean Operations (2 points)" allLiftBoolOp_prop
    , testProperty "=P= Lifts Comparison Operations (2 points)" allLiftCompOp_prop
    ]
  , testGroup "=G= eval Function"
    [ testProperty "=P= Constant Expressions (2 points)" allConstExp_prop
    , testProperty "=P= Variable Expressions (2 points)" allVarExp_prop
    
    , testProperty "=P= Integer Operation Expressions (3 points)"     allIntOpExp_prop
    , testProperty "=P= Comparison Operation Expressions (3 points)" allCompOpExp_prop
    , testProperty "=P= Boolean Operation Expressions (3 points)"    allBoolOpExp_prop

    , testProperty "=P= If Expressions (3 points)"        allIfExp_prop
    , testProperty "=P= Function Expressions (3 points)"  allFunExp_prop
    , testProperty "=P= Function Application (4 points)"  allAppExp_prop
    , testProperty "=P= Let Expressions (4 points)"       allLetExp_prop
    ]
  , testGroup "=G= exec Function"
    [ testProperty "=P= Assign Statements (2 points)"          allSetStmt_prop
    , testProperty "=P= Sequence Statements (3 points)"        allSeqStmt_prop
    , testProperty "=P= If Statements (3 points)"               allIfStmt_prop
    , testProperty "=P= Procedure Statements (2 points)"      allProcStmt_prop
    , testProperty "=P= Call Procedure Statements (6 points)" allCallStmt_prop
    ]
  ]

buildTestProp :: Testable a => [(b, a)] -> Property
buildTestProp = foldl1 conjunct . map ( property . snd ) 
  where conjunct prop assert = prop .&&. assert

-- all test expression props
allLiftBoolOp_prop =
  buildTestProp liftBoolOpUnitTests
  .&&. (forAll arbLiftBool $ liftOp_prop liftBoolOp)

allLiftCompOp_prop =
  buildTestProp liftCompOpUnitTests
  .&&. (forAll arbLiftComp $ liftOp_prop liftCompOp)


allConstExp_prop =
  buildTestProp constExpUnitTests
  .&&. forAll arbConstExp anyExpVal_prop

allVarExp_prop =
  buildTestProp varExpUnitTests
  .&&. forAll arbVarExp anyExpEnvVal_prop

allIntOpExp_prop =
  buildTestProp intOpExpUnitTests
  .&&. forAll (arbIntOpExp 3) anyExpVal_prop
      
allCompOpExp_prop =
  buildTestProp compOpExpUnitTests
  .&&. forAll (arbCompOpExp 3) anyExpVal_prop

allBoolOpExp_prop =
  buildTestProp boolOpExpUnitTests
  .&&. forAll (arbBoolOpExp 3) anyExpVal_prop

allIfExp_prop =
  buildTestProp ifExpUnitTests
  .&&. forAll arbIfExp anyExpVal_prop

allFunExp_prop =
  buildTestProp funExpUnitTests
  .&&. forAll arbFunExp anyExpEnvVal_prop

allAppExp_prop =
  buildTestProp appExpUnitTests
  .&&. forAll arbAppExp anyExpEnvVal_prop
      
allLetExp_prop =
  buildTestProp letExpUnitTests
  .&&. forAll arbLetExp anyExpEnvVal_prop

--- exec: all test props

allSetStmt_prop =
  buildTestProp setStmtUnitTests
  .&&. forAll arbSetStmt anyStmtEnvPEnvVal_prop
      
allSeqStmt_prop =
  buildTestProp seqStmtUnitTests
  .&&. forAll arbSeqStmt anyStmtEnvPEnvVal_prop

allIfStmt_prop =
  buildTestProp ifStmtUnitTests
  .&&. forAll arbIfStmt anyStmtEnvPEnvVal_prop

allProcStmt_prop =
  buildTestProp procStmtUnitTests
  .&&. forAll arbProcStmt anyStmtEnvPEnvVal_prop

allCallStmt_prop =
  buildTestProp callStmtUnitTests
  .&&. forAll arbCallStmt anyStmtEnvPEnvVal_prop
