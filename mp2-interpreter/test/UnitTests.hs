module UnitTests where
import Data.HashMap.Strict as H

import Test.Tasty.HUnit

import Lib

-- oops
assertEqual' name actual expect = assertEqual name expect actual

--- Lift Operations
liftBoolOpUnitTests :: [(String, Assertion)]
liftBoolOpUnitTests = 
  [ ( "lift &&"
    , assertEqual' ""
      ( liftBoolOp (&&) (BoolVal True) (BoolVal False) )
      $ BoolVal False
    )
  , ( "Lift ||"
    , assertEqual' "lift ||"
      ( liftBoolOp (||) (BoolVal True) (BoolVal False) )
      $ BoolVal True
    )
  , (  "lift || (exception)"
    , assertEqual' ""
      ( liftBoolOp (||) (IntVal 1) (IntVal 0) )
      $ ExnVal "Cannot lift"
    )
  ]

liftCompOpUnitTests :: [(String, Assertion)]
liftCompOpUnitTests = 
  [ ( "lift <="
    , assertEqual' ""
      ( liftCompOp (<=) (IntVal 5) (IntVal 4) )
      $ BoolVal False
    )
  , ( "lift >"
    , assertEqual' ""
      ( liftCompOp (>) (IntVal 5) (IntVal 4) )
      $ BoolVal True
    )
  , ( "lift == (exception)"
    ,  assertEqual' ""
      ( liftCompOp (==) (BoolVal True) (BoolVal True) )
      $ ExnVal "Cannot lift"
    )
  ]

--- Constant Expressions
constExpUnitTests :: [(String, Assertion)]
constExpUnitTests =
  [ ( "Int Constant"
    , assertEqual' ""
      ( eval (IntExp (-5)) H.empty )
      $ IntVal (-5)
    )
  , ( "Bool Constant"
    , assertEqual' ""
      ( eval (BoolExp True) H.empty )
      $ BoolVal True
    )
  ]

--- Variable Expressions
testenv1 :: HashMap [Char] Val
testenv1 = H.fromList [ ("x", IntVal 3)
                      , ("y", BoolVal True)
                      , ("z", ExnVal "ZzZz")
                      , ("f", CloVal [] (IntExp 0) H.empty)
                      ]

varExpUnitTests :: [(String, Assertion)]
varExpUnitTests =
  [ ( "Integer VarExp"
    , assertEqual' ""
      ( eval (VarExp "x") testenv1 )
      $ IntVal 3
    )
  , ( "VarExp (exception)"
    , assertEqual' ""
      ( eval (VarExp "x") H.empty)
      $ ExnVal "No match in env"
    )
  , ( "Cloval VarExp"
    , assertEqual' ""
      ( eval (VarExp "f") testenv1 )
      $ CloVal [] (IntExp 0) H.empty
    )
  ]

--- Operator Expressions

intOpExpUnitTests :: [(String, Assertion)]
intOpExpUnitTests =
  [ ( "(+)"
    , assertEqual' ""
      ( eval (IntOpExp "+" (IntExp 5) (IntExp 4)) H.empty )
      $ IntVal 9
    )
  , ( "(+ lift exception) "
    , assertEqual' ""
      ( eval (IntOpExp "+"
              (IntExp 6)
              (IntOpExp "/" (IntExp 4) (IntExp 0))) H.empty )
      $ ExnVal "Cannot lift"
    )
  , ( "(/)"
    , assertEqual' ""
      ( eval (IntOpExp "-"
              (IntOpExp "*" (IntExp 3) (IntExp 10))
              (IntExp 7)) H.empty )
      $ IntVal 23
    )
  , ( "(/ exception)"
    , assertEqual' ""
      ( eval (IntOpExp "/" (IntExp 6) (IntExp 0)) H.empty )
      $ ExnVal "Division by 0"
    )
  ]

boolOpExpUnitTests :: [(String, Assertion)]
boolOpExpUnitTests =
  [ ( "(or)"
    , assertEqual' ""
      ( eval (BoolOpExp "or" (BoolExp True) (BoolExp False)) H.empty )
      $ BoolVal True
    )
  , ( "(or lift exception)"
    , assertEqual' ""
      ( eval (BoolOpExp "or" (IntExp 3) (BoolExp False)) H.empty )
      $ ExnVal "Cannot lift"
    )
  ]


compOpExpUnitTests :: [(String, Assertion)]
compOpExpUnitTests =
  [ ( "(>=)"
    , assertEqual' ""
      ( eval (CompOpExp ">=" (IntExp 6) (IntExp 6)) H.empty )
      $ BoolVal True
    )
  , ( "(>= lift exception)"
    , assertEqual' ""
      ( eval (CompOpExp ">=" (BoolExp True) (IntExp 6)) H.empty )
      $ ExnVal "Cannot lift"
    )
  ]

--- If Expressions
ifExpUnitTests :: [(String, Assertion)]
ifExpUnitTests =
  [ ( "True branch"
    , assertEqual' ""
      ( eval (IfExp
               (BoolOpExp "or" (BoolExp True) (BoolExp False))
               (IntExp 5)
               (IntExp 10))
        H.empty
      )
      $ IntVal 5
    )
  , ( "False branch"
    , assertEqual' ""
      ( eval (IfExp
               (BoolOpExp "and" (BoolExp True) (BoolExp False))
               (IntExp 5)
               (IntExp 10))
        H.empty
      )
      $ IntVal 10
    )
  , ( "exception"
    , assertEqual' ""
      ( eval (IfExp
              (IntOpExp "/" (IntExp 5) (IntExp 0))
              (IntExp 5)
              (IntExp 10))
        H.empty
      )
      $ ExnVal "Condition is not a Bool"
    )
  ]

--- Function Expressions
funExpUnitTests :: [(String, Assertion)]
funExpUnitTests =
  [ ( "Multiple parameters"
    , assertEqual' ""
      ( eval (FunExp ["a", "b", "c"] (IntExp 5)) testenv1 )
      $ CloVal ["a", "b", "c"] (IntExp 5) testenv1
    )
  ]

testenv2 :: HashMap String Val
testenv2 = H.fromList
  [ ("k"
    , CloVal ["a"]
      (BoolOpExp "and" (VarExp "a") (BoolExp True)) testenv1
    )
  , ("a", IntVal 42)
  ]

appExpUnitTests :: [(String, Assertion)]
appExpUnitTests =
  [ ( "Multiple parameters"
    , assertEqual' ""
      ( eval (AppExp (FunExp ["a","b"] (IntOpExp "+" (VarExp "a") (VarExp "b")))
               [IntExp 4, IntExp 5]) H.empty )
      $ IntVal 9
    )
  , ( "non-trivial function expression"
    , assertEqual' ""
      ( eval (AppExp (VarExp "k") [BoolExp True]) testenv2 )
      $ BoolVal True
    )
  , ( "lexical scoping"
    , assertEqual' ""
      ( eval (AppExp
              (FunExp ["a"] (AppExp (VarExp "k") [VarExp "a"]))
              [BoolExp False]) testenv2 )
      $ BoolVal False
    )
  , ( "Exception"
    , assertEqual' ""
      ( eval (AppExp (VarExp "a") []) testenv2 )
      $ ExnVal "Apply to non-closure"
    )
  ]

--- Let Expressions
letExpUnitTests :: [(String, Assertion)]
letExpUnitTests =
  [ ( "Multiple bindings"
    , assertEqual' ""
      ( eval (LetExp [("a",IntExp 4),("b",IntExp 5)]
               (IntOpExp "+" (VarExp "a") (VarExp "b")))
        H.empty )
      $ IntVal 9
    )
  , ( "Empty binding"
    , assertEqual' ""
      ( eval (LetExp [] (VarExp "x")) H.empty )
      $ ExnVal "No match in env"
    )
  , ( "non-trivial binding expression"
    , assertEqual' ""
      ( eval (LetExp [("b", VarExp "k")]
               (AppExp (VarExp "b") [BoolExp True]))
        testenv2 )
      $ BoolVal True
    )
  , ( "lexical scoping"
    , assertEqual' ""
      ( eval (LetExp [("a", VarExp "k")]
               (AppExp (VarExp "a") [BoolExp True]))
        testenv2 )
      $ BoolVal True
    )
  ]


--- Set Statements
setStmtUnitTests =
  [ ( "Vanilla"
    , assertEqual' ""
      ( exec (SetStmt "x" (IntExp 5)) H.empty H.empty )
      ("", H.empty, H.fromList [("x", IntVal 5)])
    )
  , ( "Variable shadowing"
    , assertEqual' ""
      ( exec (SetStmt "x" (IntExp 5)) H.empty (H.fromList [("x", IntVal 6)]) )
      ("", H.empty, H.fromList [("x", IntVal 5)])
    )
  ]

--- Seq Statements
seqStmtUnitTests =
  [ ( "Prints"
    , assertEqual' ""
      ( exec (SeqStmt [ PrintStmt (IntExp 4)
                      , PrintStmt (IntExp 2)
                      ]
             )
        H.empty H.empty
      )
      ("42", H.empty, H.empty)
    )
  , ( "Variable Shadowing"
    , assertEqual' ""
      ( exec (SeqStmt [ PrintStmt (VarExp "a")
                      , SetStmt "a" (IntExp 24)
                      , PrintStmt (VarExp "a")])
        H.empty testenv2 )
      ("4224", H.empty, H.insert "a" (IntVal 24) testenv2)
    )
  , ( "Exception"
    , assertEqual' ""
      ( exec (SeqStmt [ PrintStmt (VarExp "x")
                      , PrintStmt (IntExp 2)
                      ]
             )
        H.empty H.empty
      )
      ("exn: No match in env2", H.empty, H.empty)
    )
  ]

--- if statements
ifStmtUnitTests =
  [ ( "Vanilla"
    , assertEqual' ""
      ( exec (IfStmt (BoolExp True)
              (PrintStmt (IntExp 5))
              (PrintStmt (IntExp 10)))
        H.empty H.empty
      )
      ("5", H.empty, H.empty)
    )
  , ( "Non-trivial conditional"
    , assertEqual' ""
      ( exec (IfStmt (BoolExp False)
              (PrintStmt (IntExp 5))
              (PrintStmt (IntExp 10)))
        H.empty H.empty
      )
      ("10", H.empty, H.empty)
    )
  , ( "Exception"
    , assertEqual' ""
      ( exec (IfStmt (FunExp [] (IntExp 0))
              (PrintStmt (IntExp 5))
              (PrintStmt (IntExp 10)))
        H.empty H.empty
      )
      ("exn: Condition is not a Bool", H.empty, H.empty)
    )
  ]

--- procedure statements
procStmtUnitTests =
  [ ( "Vanilla"
    , assertEqual' ""
      ( exec (ProcedureStmt "p" [] (PrintStmt (VarExp "x"))) H.empty H.empty )
      ( ""
      , H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (VarExp "x")))]
      , H.empty
      )
    )
  , ( "Environment preserved"
    , assertEqual' ""
      ( exec (ProcedureStmt "p" [] (PrintStmt (VarExp "x"))) H.empty testenv2 )
      ( ""
      , H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (VarExp "x")))]
      , testenv2)
    )
  ]

procFibHelp =
  ProcedureStmt "fibHelper" ["n"]
  (IfStmt (CompOpExp "<=" (VarExp "n") (IntExp 0))
    (PrintStmt $ VarExp "acc2")
    (SeqStmt [ SetStmt "tmp" $ VarExp "acc2"
             , SetStmt "acc2" $ IntOpExp "+" (VarExp "acc1") (VarExp "acc2")
             , SetStmt "acc1" $ VarExp "tmp"
             , CallStmt "fibHelper" [IntOpExp "-" (VarExp "n") (IntExp 1)]
             ]))
procFibonacci =
  ProcedureStmt "Fibonacci" ["n"]
  $ SeqStmt [ procFibHelp
            , SetStmt "acc1" $ IntExp 0
            , SetStmt "acc2" $ IntExp 1
            , CallStmt "fibHelper" [VarExp "n"]
            ]


--- call statements
callStmtUnitTests =
  [ ( "10th Fibonacci number"
    , assertEqual' ""
      ( exec (SeqStmt
              [ procFibonacci
              , CallStmt "Fibonacci" [IntExp 10]
              ])
        H.empty H.empty)
      ( "89"
      , H.fromList [("fibHelper", procFibHelp),("Fibonacci", procFibonacci)]
      , H.fromList [ ("acc1", IntVal 55), ("n", IntVal 0)
                   , ("acc2", IntVal 89), ("tmp", IntVal 55)
                   ]
      )
    )
  , ( "Higher Order Procedure"
    , assertEqual' ""
      ( exec ( SeqStmt [ ProcedureStmt "fog" ["f", "g", "x"]
                         (SetStmt "x" (AppExp (VarExp "f")
                                       [(AppExp (VarExp "g") [VarExp "x"])]))
                       , CallStmt "fog"
                         [ FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2))
                         , FunExp ["x"] (IntOpExp "+" (VarExp "x") (IntExp 1))
                         , IntExp 6
                         ]
                       , PrintStmt (VarExp "x")
                       ]
             ) H.empty H.empty
      )
      ( "14"
      , H.fromList [("fog", ProcedureStmt "fog" ["f", "g", "x"]
                      (SetStmt "x" (AppExp (VarExp "f")
                                    [(AppExp (VarExp "g") [VarExp "x"])])))
                   ]
      , H.fromList [ ("x", IntVal 14)
                   , ("f", CloVal ["x"]
                       (IntOpExp "*" (VarExp "x") (IntExp 2)) H.empty)
                   , ("g", CloVal ["x"]
                       (IntOpExp "+" (VarExp "x") (IntExp 1)) H.empty)
                   ]
      )
    )
  ]
