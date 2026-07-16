{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module Tests where

import Data.Map.Strict as H (Map, empty, fromList)
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), (@=?))
import qualified Test.Tasty.HUnit as HUnit ((@?=))

import Common
import Parser
import Infer

class TestCase t where
  name :: t -> String
  points :: t -> Int
  assertion :: t -> Assertion

type Input = String
type Output = String -- we use strings as output to get canonicalization

makeTestCase :: TestCase t => t -> TestTree
makeTestCase t = testCase ptName test
  where ptName = "=P= " ++ name t ++ " (" ++ show (points t) ++ " points)"
        test = assertion t

testCaseGroup :: TestCase t => TestName -> [t] -> TestTree
testCaseGroup name ts = testGroup ("=G= " ++ name) $ map makeTestCase ts

-- We want the tests to compare expected and actual values by testing
-- if they print the same. However there's a couple complications.
-- The @?= assertion operator from HUnit uses the Eq instance of its
-- arguments, and we also want the arguments to be printed normally
-- to the students (i.e., show 'a instead of "'a").
-- 
-- We also want to be able to write our expected output as a string,
-- so that it's easier to read, when possible.
--
-- This means we need to give arguments to @?= that are not type-symmetric,
-- and have fairly specialized Eq and Show instances. To this end we use
-- some type magic called an "existential type," as well as another class
-- for things that are test results.

class TestResult r where
  display :: r -> String

instance TestResult String where
  display = id
instance TestResult MonoTy where
  display = showMonoTyWith H.empty
instance TestResult PolyTy where
  display (Forall _ tau) = show tau
instance TestResult Substitution where
  display = showSubstitution
instance TestResult TypeError where
  display = show
instance (TestResult e, TestResult a) => TestResult (Either e a) where
  display (Left e)  = display e
  display (Right r) = display r

data PrintSame = forall r. TestResult r => PrintSame r

instance Eq PrintSame where
  PrintSame r1 == PrintSame r2 = display r1 == display r2
instance Show PrintSame where
  show (PrintSame r1) = display r1

-- (TestResult a, TestResult b) => a -> b -> Assertion
-- is more general, but we always use String on the RHS so the more specific
-- type is to help prevent mistakes.
(@?=) :: TestResult r => r -> String -> Assertion
r @?= expected = PrintSame r HUnit.@?= PrintSame expected

  -- fresh instance function testing

data FreshTest = FreshTest
  { testName    :: String
  , initCounter :: FVCounter
  , input       :: Input
  , expected    :: Output
  }

instance TestCase FreshTest where
  -- name = testName
  name (FreshTest { testName = tn }) = tn
  points _ = 0
  assertion FreshTest{..} =
    freshRun initCounter input @?= expected

freshRun :: Int -> String -> Either TypeError MonoTy
freshRun n s =
  let polyTy = parseType (lexer s)
   in runInferFrom n (freshInst polyTy)

freshGroup :: TestTree
freshGroup = testCaseGroup "freshInst"
  [ FreshTest
      { testName = "unquantified type", initCounter = 4
      , input    = "0 -> 0"
      , expected = "(0 -> 0)"
      }
  , FreshTest
      { testName = "quantified, appears more than once", initCounter = 4
      , input    = "0. 0 -> 0"
      , expected = "(4 -> 4)"
      }
  , FreshTest
      { testName = "quantified, all distinct", initCounter = 4
      , input    = "0. 1. 2. 0 * 1 * 2"
      , expected = "(4 * (5 * 6))"
      }
  , FreshTest
      { testName = "partially quantified 1", initCounter = 4
      , input    = "0. (0 * int) -> (1 * int)"
      , expected = "((4 * int) -> (1 * int))"
      }
  , FreshTest
      { testName = "partially quantified 2", initCounter = 4
      , input    = "0. 2. (0 * 2) -> (0 * 0) -> 1"
      , expected = "((4 * 5) -> ((4 * 4) -> 1))"
      }
  ]

  -- unification tests 

data UnifyTest = UnifyTest
  { testName :: String, testPoints :: Int
  , input    :: Input
  , expected :: Output
  }

instance TestCase UnifyTest where
  -- name = testName
    -- points = testPoints
  name (UnifyTest { testName = tn }) = tn
  points (UnifyTest { testPoints = tp }) = tp
  assertion UnifyTest{..} =
    unifyRun input @?= expected

unifyRun :: String -> Either TypeError Substitution
unifyRun s = runInfer $ unify $ parseEqList $ lexer s

unifyElimGroup :: TestTree
unifyElimGroup = testCaseGroup "eliminate"
  [ UnifyTest
      { testName = "single simple constraint", testPoints = 1
      , input    = "[0 ~ int]"
      , expected = "{0 -> int}"
      }
  , UnifyTest
      { testName = "two simple constraints", testPoints = 1
      , input    = "[0 ~ int, 1 ~ bool]"
      , expected = "{0 -> int, 1 -> bool}"
      }
  ]

unifyDelGroup :: TestTree
unifyDelGroup = testCaseGroup "delete"
  [ UnifyTest
      { testName = "equivalent constant types", testPoints = 1
      , input    = "[int ~ int]"
      , expected = "{}"
      }
  , UnifyTest
      { testName = "equivalent type variables", testPoints = 1
      , input    = "[0 ~ 0]"
      , expected = "{}"
      }
  , UnifyTest
      { testName = "two equivalent constraints", testPoints = 1
      , input    = "[0 ~ int, int ~ 0]"
      , expected = "{0 -> int}"
      }
  ]

unifyOrientGroup :: TestTree
unifyOrientGroup = testCaseGroup "orient"
  [ UnifyTest
      { testName = "simple inverted constraint", testPoints = 2
      , input    = "[int ~ 0]"
      , expected = "{0 -> int}"
      }
  ]

unifyDecompGroup :: TestTree
unifyDecompGroup = testCaseGroup "decompose"
  [ UnifyTest
      { testName = "function type", testPoints = 2
      , input    = "[0 -> 1 ~ int -> int]"
      , expected = "{0 -> int, 1 -> int}"
      }
  , UnifyTest
      { testName = "nested pairs", testPoints = 2
      , input    = "[0 * int ~ (1 * 2) * int, 1 ~ string, 2 ~ string]"
      , expected = "{0 -> (string * string), 1 -> string, 2 -> string}"
      }
  ]

unifyErrorGroup :: TestTree
unifyErrorGroup = testCaseGroup "unification errors"
  [ UnifyTest
      { testName = "conflicting constraints", testPoints = 1
      , input    = "[0 ~ bool, int ~ 0]"
      , expected = show $ Can'tMatch intTy boolTy
      }
  , UnifyTest
      { testName = "function passed to itself", testPoints = 2
      , input    = "[0 ~ 0 -> int]"
      , expected = show $ InfiniteType 0 (TyVar 0 `funTy` intTy)
      -- Note that the Show instance for TypeError is going to rename
      -- all of the 2s here to 'a, so don't worry about it if you somehow
      -- end up with, say, 1s instead.
      }
  , UnifyTest
      { testName = "pair contains itself", testPoints = 2
      , input    = "[0 ~ 1, 1 ~ 2, 2 ~ 0 * 0]"
      , expected = show $ InfiniteType 2 (TyVar 2 `pairTy` TyVar 2)
      }
  ]

unifyComplexGroup :: TestTree
unifyComplexGroup = testCaseGroup "harder unifications"
  [ UnifyTest
      { testName = "backsubstitute into pair", testPoints = 2
      , input    = "[0 * int ~ 1, 0 ~ string]"
      , expected = "{0 -> string, 1 -> (string * int)}"
      }
  , UnifyTest
      { testName = "function type mess", testPoints = 2
      , input    = "[0 -> 1 ~ 2, 2 ~ 3 -> 4, 0 ~ int, 4 ~ bool]"
      , expected = "{0 -> int, 1 -> bool, 2 -> (int -> bool), 3 -> int, 4 -> bool}"
      }
  ]

unifyGroup :: TestTree
unifyGroup = testGroup "unify"
  [ unifyElimGroup 
  , unifyDelGroup 
  , unifyOrientGroup 
  , unifyDecompGroup 
  , unifyErrorGroup 
  , unifyComplexGroup
  ]

  -- inference tests

data InferTest = InferTest
  { testName :: String, testPoints :: Int
  , gamma    :: Input, expr :: Input
  , expected :: Output}

inferTest :: InferTest
inferTest = InferTest
  { testName = "", testPoints = 0 
  , gamma = "{}", expr = ""
  , expected = ""
  }

instance TestCase InferTest where
  -- name = testName
  -- points = testPoints
  name (InferTest { testName = tn }) = tn
  points (InferTest { testPoints = tp }) = tp
  assertion InferTest{..} =
    inferRun gamma expr @?= expected

inferRun :: String -> String -> Either TypeError PolyTy
inferRun s1 s2 = runInferFrom 1 (inferInit gamma exp)
  where gamma = H.fromList $ parseEnv $ lexer s1
        exp   = parseExp $ lexer s2

inferConstGroup :: TestTree
inferConstGroup = testCaseGroup "constants"
  [ inferTest
      { testName = "int", testPoints = 1
      , expr     = "46"
      , expected = "int"
      }
  , inferTest
      { testName = "bool", testPoints = 1
      , expr     = "true"
      , expected = "bool"
      }
  , inferTest
      { testName = "unit", testPoints = 1
      , expr     = "()"
      , expected = "unit"
      }
  , inferTest
      { testName = "list", testPoints = 2
      , expr     = "[]"
      , expected = "'a list"
      }
  ]

inferVarGroup :: TestTree
inferVarGroup = testCaseGroup "vars"
  [ inferTest
      { testName = "not in scope", testPoints = 1
      , expr     = "x"
      , expected = show $ LookupError "x"
      }
  , InferTest
      { testName = "monotype", testPoints = 2
      , gamma    = "{f -> int}", expr = "f"
      , expected = "int"
      }
  , InferTest
      { testName = "polytype", testPoints = 2
      , gamma    = "{x -> 0. 0}", expr = "x"
      , expected = "'a"}
  ]

inferLetGroup :: TestTree
inferLetGroup = testCaseGroup "let bindings"
  [ inferTest
      { testName = "unused binding", testPoints = 1
      , expr     = "let x = 0 in 0"
      , expected = "int"
      }
  , inferTest
      { testName = "body has same type as binding", testPoints = 2
      , expr     = "let f = 1 in f"
      , expected = "int"
      }
  , InferTest
      { testName = "shadowing", testPoints = 1
      , gamma    = "{x -> bool}", expr = "let x = 0 in x"
      , expected = "int"
      }
  , inferTest
      { testName = "empty list", testPoints = 1
      , expr     = "let h = [] in h"
      , expected = "'a list"
      }
  , inferTest
      { testName = "generalize and instantiate", testPoints = 2
      , expr     = "let h = [] in (h, h)"
      , expected = "('a list * 'b list)"
      }
  , inferTest
      { testName = "don't overgeneralize (depends on fun)", testPoints = 2
      , expr     = "fun x -> let y = x + 1 in y"
      , expected = "(int -> int)"
      }
  ]

inferOpGroup :: TestTree
inferOpGroup = testCaseGroup "operators"
  [ inferTest
      { testName = "plus", testPoints = 1
      , expr     = "6 + 6"
      , expected = "int"
      }
  , inferTest
      { testName = "cons", testPoints = 2
      , expr     = "6 :: []"
      , expected = "int list"
      }
  , InferTest
      { testName = "snd", testPoints = 2
      , gamma    = "{x -> string}"
      , expr     = "snd ((print x), (\"6\" ^ \"boohoo\"))"
      , expected = "string"
      }
  , inferTest
      { testName = "monomorphic mess", testPoints = 1
      , expr     = "not ((~ 7 * 8 / 1 - 4) > 3)"
      , expected = "bool"
      }
  , inferTest
      { testName = "separate instantiations", testPoints = 3
      , expr     = "fst ((6 = hd (7 :: [])), tl (true :: []))"
      , expected = "bool"
      }
  ]

inferCondGroup :: TestTree
inferCondGroup = testCaseGroup "conditionals"
  [ inferTest
      { testName = "simple and correct", testPoints = 2
      , expr     = "if true then 62 else 252"
      , expected = "int"
      }
  , inferTest
      { testName = "constrain branches", testPoints = 2
      , expr     = "if false then (7 :: []) else []"
      , expected = "int list"
      }
  , inferTest
      { testName = "branches disagree", testPoints = 2
      , expr     = "if true then 0 else \"hello\""
      , expected = show $ Can'tMatch intTy stringTy
      }
  , inferTest
      { testName = "condition is not a bool", testPoints = 2
      , expr     = "if 0 then 1 else 2"
      , expected = show $ Can'tMatch intTy boolTy
      }
  ]

inferFunGroup :: TestTree
inferFunGroup = testCaseGroup "functions"
  [ inferTest
      { testName = "plusOne", testPoints = 3
      , expr     = "fun x -> x + 1"
      , expected = "(int -> int)"
      }
  , inferTest
      { testName = "const (const 1)", testPoints = 2
      , expr     = "fun x -> fun y -> 1"
      , expected = "('a -> ('b -> int))"
      }
  , InferTest
      { testName = "captured variable", testPoints = 3
      , gamma    = "{x -> 7}", expr = "fun y -> y + x"
      , expected = "(int -> int)"
      }
  , inferTest
      { testName = "id", testPoints = 2
      , expr     = "fun x -> x"
      , expected = "('a -> 'a)"
      }
  ]

inferAppGroup :: TestTree
inferAppGroup = testCaseGroup "applications"
  [ InferTest
      { testName = "simple monomorphic function", testPoints = 2
      , gamma    = "{f -> int -> int}"
      , expr     = "f 0"
      , expected = "int"
      }
  , InferTest
      { testName = "bad monomorphic function", testPoints = 1
      , gamma    = "{f -> string -> int}"
      , expr     = "f 0"
      , expected = show $ Can'tMatch stringTy intTy
      }
  , inferTest
      { testName = "apply lambda", testPoints = 1
      , expr     = "(fun x -> x + 1) 6"
      , expected = "int"
      }
  , InferTest
      { testName = "monomorphic curried application", testPoints = 1
      , gamma    = "{g -> int -> int -> int}"
      , expr     = "g 1 2"
      , expected = "int"
      }
    -- in the next two, we have to be especially careful. We are putting non-quantified
    -- variables in the environment, so we need to make sure they won't get captured by
    -- a fresh variable when 'infer' is called. This is why inferRun uses
    -- @runInferFrom 1@ instead of simply 'runInfer'.
  , InferTest
      { testName = "function has type variable", testPoints = 1
      , gamma    = "{f -> 0 -> 0}"
      , expr     = "f 0"
      , expected = "int"
      }
  , InferTest
      { testName = "arguments don't agree", testPoints = 2
      , gamma    = "{f -> 0 -> 0 -> 0}"
      , expr     = "f 0 false"
      , expected = show $ Can'tMatch intTy boolTy
      }
  , InferTest
      { testName = "polymorphic HOF", testPoints = 2
      , gamma    = "{f -> 0 . 1 . (0 -> 1) -> 0 -> 1, g -> int -> int}"
      , expr     = "f g"
      , expected = "(int -> int)"
      }
  , InferTest
      { testName = "bad polymorphic application", testPoints = 2
      , gamma    = "{f -> 0 . 0 -> 0 -> bool}" -- e.g. ==
      , expr     = "f 0 false"
      , expected = show $ Can'tMatch intTy boolTy
      }
  ]

inferLetrecGroup :: TestTree
inferLetrecGroup = testCaseGroup "recursive bindings"
  [ inferTest
      { testName = "ill-defined binding", testPoints = 2
      , expr     = "let rec f x = f x in f false"
      , expected = "'a"
      }
  , inferTest
      { testName = "useless recursive function", testPoints = 2
      , expr     = "let rec f x = if 0 > x then (f (x - 1)) + 1 else 1 in f 5"
      , expected = "int"
      }
  , inferTest
      { testName = "apply argument to self", testPoints = 1
      , expr     = "let rec f g = g f in 0"
      , expected = show $ InfiniteType 0 
                            ((TyVar 0 `funTy` TyVar 1) `funTy` TyVar 1)
      }
  , inferTest
      { testName = "omega combinator", testPoints = 2
      , expr     = "let rec f x = x x in f"
      , expected = show $ InfiniteType 0 (TyVar 0 `funTy` TyVar 1)
      }
  , inferTest
      { testName = "generalization", testPoints = 3
      , expr     = "let rec f x = x in ((f 0), (f false))"
      , expected = "(int * bool)"
      }
  ]

inferGroup :: TestTree
inferGroup = testGroup "infer"
  [ inferConstGroup, inferVarGroup, inferLetGroup, inferOpGroup
  , inferCondGroup, inferFunGroup, inferAppGroup, inferLetrecGroup
  ]
