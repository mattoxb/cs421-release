import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck ()

import Data.Foldable ()
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)

import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Timeout (timeout)
import Control.Exception (try, evaluate, SomeException)

import Tests


main :: IO ()    
main = putStrLn "" >> defaultMain tests

tests = testGroup "User Friendly Tests"
  [ unitTests ]
