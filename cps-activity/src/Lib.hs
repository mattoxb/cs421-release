module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)


calc :: Num a => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc = undefined

