{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Foldable
import qualified Data.List as List
import GHC.Generics (Generic)

import Lib

main :: IO ()
main = defaultMain tests

tests =
  [  testGroup
      "list2cons Function"
      [ testProperty
          "Converts [a] to (List a) exactly"
          (prop_list2cons_eq_foldableToCons :: [Integer] -> Property)
      ]
  , testGroup
      "cons2list Function"
      [ testProperty
          "Converts (List a) to [a] exactly"
          (prop_cons2list_eq_foldableToList :: List Integer -> Property)
      ]
  , testGroup
      "eval Function"
      [ testProperty
          "Evaluates expressions correctly"
          (mapSize (`div` 8) prop_eval_is_proper)
      ]
  , testGroup
      "list2cons' Function"
      [ testProperty
          "Converts [a] to (List a) exactly"
          (prop_list2consP_eq_foldableToCons :: [Integer] -> Property)
      ]
  , testGroup
      "sumTree Function"
      [ testProperty
          "Sums all values in the tree"
          (prop_sumTree_eq_foldableSum :: BinTree Integer -> Property)
      ]
  , testGroup
      "liftIntOp Function"
      [ testProperty
          "Lifts integer operations correctly"
          prop_liftIntOp_shows_properly
      ]
  ]

--------------------------------
-- Algebraic Datatypes: List --
prop_list2cons_eq_foldableToCons :: (Eq a, Show a) => [a] -> Property
prop_list2cons_eq_foldableToCons xs = list2cons xs === toCons xs

prop_cons2list_eq_foldableToList :: (Eq a, Show a) => List a -> Property
prop_cons2list_eq_foldableToList xz = cons2list xz === toList xz

prop_list2consP_eq_foldableToCons :: (Eq a, Show a) => [a] -> Property
prop_list2consP_eq_foldableToCons xs = list2cons' xs === toCons xs

------------------------------
-- Algebraic Datatypes: Exp --
prop_eval_is_proper :: Exp -> Property
prop_eval_is_proper xp = eval xp === evalProper xp
  where
    evalProper :: Exp -> Integer
    evalProper (IntExp n)    = n
    evalProper (PlusExp xps) = sum $ map evalProper xps
    evalProper (MultExp xps) = product $ map evalProper xps

deriving instance Foldable BinTree
deriving instance Generic (BinTree a)
instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = sized $ \s ->
    if s <= 0
    then return Leaf
    else oneof
         [ return Leaf
         , do n <- arbitrary
              l <- scale (\s -> (s - 1) `max` 0) arbitrary
              r <- scale (\s -> (s - 1) `max` 0) arbitrary
              return $ Node n l r
         ]
  shrink = genericShrink

prop_sumTree_eq_foldableSum :: (Eq a, Num a, Show a) => BinTree a -> Property  
prop_sumTree_eq_foldableSum xs = sumTree xs === sum xs  

prop_liftIntOp_shows_properly ::
     Fun (Integer, Integer) Integer -> SimpVal -> SimpVal -> Property
prop_liftIntOp_shows_properly f xv yv =
  show (liftIntOp f' xv yv) === show (liftIntOpProper f' xv yv)
  where
    liftIntOpProper
      :: (Integer -> Integer -> Integer)
      ->  SimpVal -> SimpVal -> SimpVal
    liftIntOpProper f (IntVal i1) (IntVal i2) = IntVal $ f i1 i2
    liftIntOpProper _ _ _                     = ExnVal "not an IntVal!"
    f' = applyFun2 f

deriving instance Foldable List

toCons :: Foldable t => t a -> List a
toCons = foldr Cons Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap toCons (arbitrary :: Gen [a])
  shrink = map toCons . shrink . toList

deriving instance Generic Exp

instance Arbitrary Exp where
  arbitrary =
    oneof
      [ IntExp <$> arbitrary
      , PlusExp <$> scale (\s -> (s - 1) `max` 0) arbitrary
      , MultExp <$> scale (\s -> (s - 1) `max` 0) arbitrary
      ]
  shrink = genericShrink

deriving instance Generic SimpVal

instance Arbitrary SimpVal where
  arbitrary =
    oneof
      [ IntVal <$> arbitrary
      , BoolVal <$> arbitrary
      , StrVal <$> arbitrary
      , return $ ExnVal "not an IntVal!"
      , ExnVal <$> arbitrary
      ]
  shrink = genericShrink
