--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval
import Scheme.Runtime
import Text.ParserCombinators.Parsec (parse)
import Data.HashMap.Strict (empty, fromList)
import Data.List (isInfixOf,intercalate)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Test.Tasty.HUnit ( Assertion, assertEqual, assertFailure, testCase )
import Test.Tasty ( defaultMain, testGroup, TestTree )


withRuntime :: [String] -> [String]
withRuntime codes = aux runtime codes
  where aux runtime [] = []
        aux runtime (code:codes) =
          case parse exprP "Expression" code of                   -- Parse
            Left err -> [show err]                          -- Diagnostics
            Right expr ->
              case runExcept $ runStateT (eval expr) runtime of   -- Eval
                Left err -> [err_type err]                      -- Diagnostics
                Right (val, env) -> show val : aux env codes

assertEval :: [String] -> [String] -> Assertion
assertEval exps expected = 
  let actual = withRuntime exps in 
  if actual == expected then 
    return ()
  else 
    assertFailure $ "Incorrect behavior for the following interaction:\n" ++ errorMsg exps actual expected

errorMsg :: [String] -> [String] -> [String] -> String
errorMsg (exp:exps) (actual:actuals) (expected:expecteds)
  | actual == expected = "scheme> " ++ exp ++ "\n" ++ actual ++ "\t\t ; OK\n" ++ errorMsg exps actuals expecteds
  | otherwise = "scheme> " ++ exp ++ "\n" ++ actual ++ "\t\t ; expected " ++ result ++ "\t\t\n"
      where result = if expected == "" then "nothing" else expected

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testGroup "=G= Runtime Tests"
    [arithRuntimeTests, boolRuntimeTests, compRuntimeTests, listOpsTests,
    unaryRuntimeTests, equalityTests, moduloTests, typePredTests] 
  , testGroup "=G= Evaluator Tests"
    [atomTests, lookupTests, defineTests1, defineTests2, hofTests, condTests,
    letTests, quoteEvalTests, defmacroTests, evalTests, extraRuntimeTests] 
  ]

--- Runtime
--- ----------

--- ### Arithmetic Operators

arithRuntimeTests :: TestTree
arithRuntimeTests = 
  testCase "=P= Arithmetic runtime tests (1 points)" $ do
    assertEval ["(+)"] ["0"]
    assertEval ["(-)"] ["0"]
    assertEval ["(*)"] ["1"]
    assertEval ["(+ 3 2 5)"] ["10"]
    assertEval ["(- 3 4 5)"] ["-6"]
    assertEval ["(* 7 8 10)"] ["560"]
  

--- ### Boolean Operators

boolRuntimeTests :: TestTree
boolRuntimeTests = 
  testCase "=P= Boolean runtime tests (1 points)" $ do
    assertEval ["(and #t #t #t #t)"] ["#t"]
    assertEval ["(and #t #t #t #t)"] ["#t"]
    assertEval ["(and #t #t 'nil)"] ["#t"]
    assertEval ["(and 't 't #f #t)"] ["#f"]
    assertEval ["(and 't 't 't 't 't 't 't 't 't 't #f #t)"] ["#f"]
    assertEval ["(and 3 4 2 't)"] ["#t"]
    assertEval ["(and 3 4 2 3 4 2 3 4 2 3 4 2 3 4 2 't)"] ["#t"]
    assertEval ["(or #t #t #t #t)"] ["#t"]
    assertEval ["(or #t #t #t #f)"] ["#t"]
    assertEval ["(and)"] ["#t"]
    assertEval ["(or)"] ["#f"]
    assertEval ["(or 'nil 3 5 #t)"] ["#t"]

--- ### Comparison Operators

compRuntimeTests :: TestTree
compRuntimeTests = 
  testCase "=P= Comparison tests (1 points)" $ do
    assertEval ["(<)"] ["#t"]
    assertEval ["(>)"] ["#t"]
    assertEval ["(<=)"] ["#t"]
    assertEval ["(>=)"] ["#t"]
    assertEval ["(=)"] ["#t"]
    assertEval ["(< 3 4 5)"] ["#t"]
    assertEval ["(< 3 3 5)"] ["#f"]
    assertEval ["(> 3 4 5)"] ["#f"]
    assertEval ["(> 100 10 3 1 0)"] ["#t"]
    assertEval ["(= 3 3 3)"] ["#t"]
    assertEval ["(= 3 3 1)"] ["#f"]



--- ### List Operators
listOpsTests :: TestTree
listOpsTests = 
  testCase "=P= List tests (1 points)" $ do
    assertEval ["(car '(10))"] ["10"]
    assertEval ["(cdr '(10 20 30))"] ["(20 30)"]
    assertEval ["(cdr '(10 20 . 30))"] ["(20 . 30)"]
    assertEval ["(cdr '(10 . (20 30)))"] ["(20 30)"]
    assertEval ["(cdr '(10 . 'a))"] ["(quote a)"]
    assertEval ["(cdr '(10))"] ["()"]
    assertEval ["(cons 10 20)"] ["(10 . 20)"]
    assertEval ["(car (cons 10 (cons 20 '())))"] ["10"]
    assertEval ["(car (cons 2 3))"] ["2"]
    assertEval ["(cdr (cons 2 3))"] ["3"]
    assertEval ["(list 'a 'b 'z 'd 'q)"] ["(a b z d q)"]
    assertEval ["(list (+ 10 20) (+ 30 40))"] ["(30 70)"]
    assertEval ["(cons 10 (list 20 30))"] ["(10 20 30)"]
    assertEval ["(cons '() '(. ()))"] ["(())"]


--- ### Unary Boolean Operators
unaryRuntimeTests :: TestTree
unaryRuntimeTests = 
  testCase "=P= Unary boolean operators (1 points)" $ do
    assertEval ["(not)"] ["ERROR: unexpected_args"]
    assertEval ["(not 77 2)"] ["ERROR: unexpected_args"]
    assertEval ["(not #t)"] ["#f"]
    assertEval ["(not #t)"] ["#f"]
    assertEval ["(not '#f)"] ["#t"]
    assertEval ["(not #f)"] ["#t"]
    assertEval ["(not #f #t)"] ["ERROR: unexpected_args"]
    assertEval ["(not #f #t 3)"] ["ERROR: unexpected_args"]
    assertEval ["(not #f 3)"] ["ERROR: unexpected_args"]
    assertEval ["(not 42)"] ["#f"]


equalityTests :: TestTree
equalityTests = 
  testCase "=P= Equality tests (1 points)" $ do
    assertEval ["(=)"] ["#t"]
    assertEval ["(eq?)"] ["#t"]
    assertEval ["(= 10 20)"] ["#f"]
    assertEval ["(= 10 10)"] ["#t"]
    assertEval ["(= 10 10 10)"] ["#t"]
    assertEval ["(= 10 10 10 10 10 10 10 10 10 10 10 10 ((lambda (x) 10) 1) 10 10 10)"] ["#t"]
    assertEval ["(= 10 10 10 10 30 10 10 10)"] ["#f"]
    assertEval ["(= 'a 'a)"] ["ERROR: type_error"]
    assertEval ["(define x ((lambda () (+ 20 100))))", "(= ''120 ''120)"] ["", "ERROR: type_error"]
    assertEval ["(define x ((lambda () (+ 20 100))))", "(define y x)", "(= x 120)", "(= x y)", "(= 'y 'y)"] ["", "", "#t", "#t", "ERROR: type_error"]
    assertEval ["(= (lambda (x) (x)) (lambda (x) (x)))"] ["ERROR: type_error"]
    assertEval ["(eq? 'a 'a)"] ["#t"]
    assertEval ["(eq? 'a 'a 'a)"] ["#t"]
    assertEval ["(eq? 'a 'a 'b 'a (car (cons 'a 'b)))"] ["#f"]
    assertEval ["(eq? '() '())"] ["#f"]
    assertEval ["(define a (- (- 10)))", "(eq? 10 a)"] ["", "#t"]
    assertEval ["(eq? (lambda () ()) (lambda (f) (lambda () ())))"] ["#f"]
    assertEval ["(eq? ((lambda () 1)) (((lambda () (lambda () 1)))))"] ["#t"]


moduloTests :: TestTree
moduloTests = 
  testCase "=P= Modulo tests (1 points)" $ do
    assertEval ["(modulo)"] ["ERROR: unexpected_args"]
    assertEval ["(modulo 1)"] ["ERROR: unexpected_args"]
    assertEval ["(modulo 4 3)"] ["1"]
    assertEval ["(modulo 9 3)"] ["0"]
    assertEval ["(modulo 9 (- 2))"] ["-1"]
    assertEval ["(modulo (- 9) 2)"] ["1"]


typePredTests :: TestTree
typePredTests = 
  testCase "=P= Type predicate tests (1 points)" $ do
    assertEval ["(symbol? 'a)"] ["#t"]
    assertEval ["(symbol? 'b)"] ["#t"]
    assertEval ["(define (caddddddr x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))", "(define sentence '(Can you cancel this MP ? . (No)))", "sentence", "(symbol? (caddddddr sentence))"] ["", "", "(Can you cancel this MP ? No)", "#t"]
    assertEval ["(symbol?)"] ["ERROR: unexpected_args"]
    assertEval ["(symbol? 3)"] ["#f"]
    assertEval ["(symbol? (car ((lambda (x) (cons x '(3 5))) 'y)))"] ["#t"]
    assertEval ["(list? '(3 5))"] ["#t"]
    assertEval ["(list? '())"] ["#t"]
    assertEval ["(list? '(3 . (6 . 7)))"] ["#f"]
    assertEval ["(list? '(3 . (6 . (1 2 3))))"] ["#t"]
    assertEval ["(list? '(3 . (6 . ())))"] ["#t"]
    assertEval ["(list? '(3 5 . 6))"] ["#f"]
    assertEval ["(list? '(3 5 (6 . 7)))"] ["#t"]
    assertEval ["(list? 3)"] ["#f"]
    assertEval ["(list? 3 5)"] ["ERROR: unexpected_args"]
    assertEval ["(list?)"] ["ERROR: unexpected_args"]
    assertEval ["(pair?)"] ["ERROR: unexpected_args"]
    assertEval ["(pair? 3)"] ["#f"]
    assertEval ["(pair? '(3 . 6))"] ["#t"]
    assertEval ["(pair? ((lambda (x) (cons x '(3 5))) 'x))"] ["#t"]
    assertEval ["(number? '(3))"] ["#f"]
    assertEval ["(pair? '())"] ["#f"]
    assertEval ["(pair? 1 2)"] ["ERROR: unexpected_args"]
    assertEval ["(number? ((lambda () 3)))"] ["#t"]
    assertEval ["(boolean? (lambda (x) 10))"] ["#f"]
    assertEval ["(boolean? #f)"] ["#t"]
    assertEval ["(boolean? 'True)", "(boolean? ''#t)", "(boolean? ''#f)"] ["#f", "#f", "#f"]
    assertEval ["(number? 10)"] ["#t"]
    assertEval ["(define n ((lambda (x) (+ 1 x)) 3))", "(number? n)"] ["", "#t"]
    assertEval ["(number? #t)"] ["#f"]
    assertEval ["(number?)"] ["ERROR: unexpected_args"]
    assertEval ["(boolean?)"] ["ERROR: unexpected_args"]
    assertEval ["(boolean? 3 #f)"] ["ERROR: unexpected_args"]
    assertEval ["(null? '())"] ["#t"]
    assertEval ["(null? '('()))"] ["#f"]
    assertEval ["(null? '(3 5))"] ["#f"]
    assertEval ["(null? '(. (. ())))"] ["#t"]
    assertEval ["(null? (cons '() '(. ())))"] ["#f"]
    assertEval ["(null?)"] ["ERROR: unexpected_args"]
    assertEval ["(null? '() '())"] ["ERROR: unexpected_args"]


--- Evaluation
--- ----------

atomTests :: TestTree
atomTests = 
  testCase "=P= Atoms tests (1 points)" $ do
    assertEval ["10"] ["10"]
    assertEval ["#t", "#f"] ["#t", "#f"]


lookupTests :: TestTree
lookupTests = 
  testCase "=P= Lookup tests (1 points)" $ do
    assertEval ["+", "-"] ["#<primitive>", "#<primitive>"]
    assertEval ["Mattox"] ["ERROR: undef_symbol"]


defineTests1 :: TestTree
defineTests1 = 
  testCase "=P= Define variable tests (1 points)" $ do
    assertEval ["(define x 10)","x","y"] ["","10","ERROR: undef_symbol"]
    assertEval ["(define x (+ 6 5))","x"] ["","11"]


defineTests2 :: TestTree
defineTests2 = 
  testCase "=P= Define function tests (1 points)" $ do
    assertEval ["(define (id x) x)","(id 10)"] ["","10"]
    assertEval ["(define (inc y) (+ y 1))","(inc 10)"] ["","11"]
    assertEval ["(define (plus a b) (+ a b))","(plus 10 20)","(plus (plus 10 20) (plus 30 40))"] ["","30","100"]



lambdaTests :: TestTree
lambdaTests = 
  testCase "=P= Lambda tests (1 points)" $ do
    assertEval ["(lambda (x) (+ x 10))"] ["#<function:(lambda (x) ...)>"]
    assertEval ["((lambda (x) (+ (((lambda () (lambda () x)))) 10)) 20)"] ["30"]
    assertEval ["(lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg))))))"] ["#<function:(lambda (X) ...)>"]
    assertEval ["(lambda X (lambda (f) (f X)))"] ["ERROR: invalid_special_form"]
    assertEval ["(define foo (lambda (x) (+ 10 x)))","(foo 20)"] ["","30"]


hofTests :: TestTree
hofTests = 
  testCase "=P= HOF tests (1 points)" $ do
    assertEval ["(define (twice f x) (f (f x)))","(define (inc x) (+ x 10))","(twice inc 10)"] ["","","30"]
    assertEval ["(define (twice f x) (f (f x)))","(define g ((lambda () twice)))","(define (inc x) (+ x 10))","(g inc 10)"] ["","","","30"]
    assertEval ["(define Y (lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg)))))))", "(define fact (Y (lambda (f) (lambda (n) (cond ((= n 0) 1) (else (* n (f (- n 1)))))))))", "(fact 10)"] ["", "", "3628800"]


condTests :: TestTree
condTests = 
  testCase "=P= Cond tests (1 points)" $ do
    assertEval ["(cond (#f 1) (#f 2))"] [""]
    assertEval ["(cond (else 1) (#t 3))"] ["ERROR: invalid_special_form"]
    assertEval ["(cond)"] ["ERROR: invalid_special_form"]
    assertEval ["(cond ('a 1) ((+ 1 2) (+ 3 4)) (else 5))"] ["1"]
    assertEval ["(cond ((+ 4 3) 'a) ((- 4 2) 'b))"] ["a"]
    assertEval ["(cond (#f 'a) ((- 4 2) 'b))"] ["b"]
    assertEval ["(cond ((+ 4 3) 'a) ((- 4 2) 'b))"] ["a"]
    assertEval ["(cond (False 'a) ((- 4 2) 'b))"] ["ERROR: undef_symbol"]
    assertEval ["(cond (True 'a) ((- 4 2) 'b))"] ["ERROR: undef_symbol"]
    assertEval ["(cond ((not 'a) 1) ((+ 1 2) (+ 3 4)) (else 5))"] ["7"]
    assertEval ["(define (fact n) (cond ((< n 1) 1) (else (* n (fact (- n 1))))))","(fact 5)"] ["","120"]
    assertEval ["(cond (#f 'a) (#f (cond (#f 1))) (((lambda () 'a)) (cond ((+ 1 2 3) 7))))"] ["7"]


letTests :: TestTree
letTests = 
  testCase "=P= Let tests (1 points)" $ do
    assertEval ["(let ((x 5) (y 10)) (+ x y)) "] ["15"]
    assertEval ["(define x 20)","(define y 30)","(let ((x 11) (y 4)) (- (* x y) 2))"] ["","","42"]
    assertEval ["(define x 20)","(define y 30)","(let ((x 11) (y 4)) (- (* x y) 2))","x","y"] ["","","42","20","30"]
    assertEval ["(define x 20)","(define y 30)","(let ((x 11) (y x)) (- (* x y) 2))"] ["","","218"]
    assertEval ["(define x 20)","(define y 30)","(let ((x 11) (y x)) (- (* x y) 2))","x","y"] ["","","218","20","30"]


letStarTests :: TestTree
letStarTests = 
  testCase "=P= Let-star tests (1 points)" $ do
    assertEval ["(let* () 5) "] ["5"]
    assertEval ["(let* ((x 5) (y 10)) (+ x y)) "] ["15"]
    assertEval ["(define x 20)","(define y 30)","(let* ((x 11) (y x)) (- (* x y) 2))"] ["","","119"]
    assertEval ["(define x 20)","(define y 30)","(let* ((x 11) (y x)) (- (* x y) 2))","x","y"] ["","","119","20","30"]
    -- This is constructing and calling a recursive 'fac' using Y combinator
    assertEval ["(define (Y x) ( (lambda (f) (x (lambda (arg) ((f f) arg)))) (lambda (f) (x (lambda (arg) ((f f) arg)))) ))",
                "(define (facx f) (lambda (n) (cond ((= n 0) 1) (else (* n (f (- n 1)))))))", "(let* ((fac (Y facx))) (fac 10))"] ["", "", "3628800"]


quoteEvalTests :: TestTree
quoteEvalTests = 
  testCase "=P= Eval tests (1 points)" $ do
    assertEval ["'a"] ["a"]
    assertEval ["'5"] ["5"]
    assertEval ["(quote a)"] ["a"]
    assertEval ["'*first-val*"] ["*first-val*"]
    assertEval ["''a"] ["(quote a)"]
    assertEval ["(car (quote (a b c)))"] ["a"]
    assertEval ["(define (make-list x y z) `(,x ,y ,z))", "(car (make-list ((lambda (b) b) 'a) 'b 'c))"] ["", "a"]
    assertEval ["(car '(a b . c))"] ["a"]
    assertEval ["(car ''(a b c))"] ["quote"]
    assertEval ["'(2 3 4)"] ["(2 3 4)"]
    assertEval ["(list (+ 2 3))"] ["(5)"]
    assertEval ["'( (+ 2 3))"] ["((+ 2 3))"]
    assertEval ["'(+ 2 3)"] ["(+ 2 3)"]
    assertEval ["(eval '(+ 1 2))"] ["3"]
    assertEval ["(eval ''(+ 1 2))"] ["(+ 1 2)"]
    assertEval ["(eval (eval ''(+ 1 2)))"] ["3"]
    assertEval ["(define a '(+ x 1))","(define x 5)","(eval a)","(define a 5)","``(+ ,,a 1)","``(+ ,,a ,a)","`(+ a ,,a)","``(+ a ,,a)","(eval ``(+ ,,a 1))","(eval (eval ``(+ ,,a 1)))"] ["","","6","","(quasiquote (+ (unquote 5) 1))","(quasiquote (+ (unquote 5) (unquote a)))","ERROR: unquote_notin_quasiquote"]
    assertEval ["(define a '(+ x 1))","(define x 5)","(eval a)","(define a 5)","``(+ ,,a 1)","``(+ ,,a ,a)","``(+ a ,,a)","(eval ``(+ ,,a 1))","(eval (eval ``(+ ,,a 1)))"] ["","","6","","(quasiquote (+ (unquote 5) 1))","(quasiquote (+ (unquote 5) (unquote a)))","(quasiquote (+ a (unquote 5)))","(+ 5 1)","6"]


defmacroTests :: TestTree
defmacroTests = 
  testCase "=P= Macro tests (1 points)" $ do
    assertEval ["(define-macro (if con then else) `(cond (,con ,then) (else ,else)))","if","(define a 5)","(if (> a 2) 10 20)","(if (< a 2) 10 20)","(define (fact n) (if (< n 1) 1 (* n (fact (- n 1)))))","(fact 10)"] ["","#<macro (con then else) ...>","","10","20","","3628800"]
    assertEval ["(define-macro (if con then else) `(cond (,con ,then) (else ,else)))","(define-macro (mkplus e) (if (eq? (car e) '-) (cons '+ (cdr e)) e))","mkplus","(mkplus (- 5 4))"] ["","","#<macro (e) ...>","9"]
    assertEval ["(define-macro (if con then else) `(cond (,con ,then) (else else)))", "(if #f 2 3)"] ["", "ERROR: undef_symbol"]


evalTests :: TestTree
evalTests = 
  testCase "=P= Eval tests (1 points)" $ do
    assertEval ["(eval 1)"] ["1"]
    assertEval ["(eval 'a)"] ["ERROR: undef_symbol"]
    assertEval ["(eval '(lambda (f x) (f x)))"] ["#<function:(lambda (f x) ...)>"]
    assertEval ["(eval '(eval '(eval '(+ ((lambda (x y z) 1) '() '() '()) 2 3))))"] ["6"]


extraRuntimeTests :: TestTree
extraRuntimeTests = 
  testCase "=P= Other runtime tests (1 points)" $ do
    assertEval ["(pair? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(1 2)))))))"] ["#t"]
    assertEval ["(pair? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(1 . 2)))))))"] ["#t"]
    assertEval ["(pair? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 zoinks))))))"] ["ERROR: undef_symbol"]
    assertEval ["(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(1 2)))))))"] ["#t"]
    assertEval ["(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(1 . 2)))))))"] ["#f"]
    assertEval ["(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 zoinks))))))"] ["ERROR: undef_symbol"]
    assertEval ["(null? '(. (. (. (.  (.  () ))))))"] ["#t"]
    assertEval ["(null? '(. (. (. (.  (.  1 ))))))"] ["#f"]
    assertEval ["(null? '(. (. (. (.  ( 1 . 1 ))))))"] ["#f"]
    assertEval ["(null? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 zoinks))))))"] ["ERROR: undef_symbol"]

