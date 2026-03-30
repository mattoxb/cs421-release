import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Debug.Trace

import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Calc works with only adds or subs." [
                testProperty "alladds" alladds
              , testProperty "allsubs" allsubs
           ]
      , testGroup "Calc uses continuations properly." [
                testProperty "usesContinuations1" (expectFailure usesContinuations1)
              , testProperty "usesContinuations2" (expectFailure usesContinuations2)
              , testProperty "correctOrdering" correctOrdering
           ]
      ]

data PosInt = PosInt Integer
 deriving (Eq,Show,Ord)

instance Num PosInt where
  (PosInt i) + (PosInt j) = PosInt (i + j)
  (PosInt i) - (PosInt j) = PosInt (check $ i - j)
      where check n = if n < 0 then error "Integer went below zero!"
                               else i - j
  fromInteger i = PosInt i

alladds b = 
  let result = calc (map Add b) 10 id id
   in result == foldr (+) 10 b
 where types = (b :: [Integer])

allsubs b = 
  let result = calc (map Sub b) 10 id id
   in result == foldl (-) 10 b
 where types = (b :: [Integer])

usesContinuations1 =
  calc [Add 1] 0 (\x -> error "") id == 1

usesContinuations2 =
  calc [Add 1] 0 id (\x -> error "") == 1

toCalc i | i < 0     = Sub (PosInt  (-i))
         | otherwise = Add (PosInt  i)

correctOrdering xx =
  let trim = map (\x -> x `mod` 1000 - 500) xx
      fixed = if sum trim < 0 then map negate trim else trim
      correct = sum fixed
      input = map toCalc fixed
      result = calc input (PosInt 0) id id 
   in  PosInt correct == result
  where types = (xx :: [Integer])

