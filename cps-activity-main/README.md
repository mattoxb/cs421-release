# cps


Suppose you have a calculator which has an accumulator and a list of
instructions.  We have a type `Calc` with two constructors: `Add i` represents
the command to add `i` to the accumulator, and `Sub i` represents the command
to subtract `i` from the accumulator.

```haskell
data Num a => Calc a = Add a
                     | Sub a
    deriving (Eq,Show)
```

The only problem is that our accumulator cannot be negative, since we want to run
this on a machine that has unsigned integers only!  Use continuations to fix this.

Here's the original calculator:

```haskell
calc xx init = aux init xx
  where aux a [] = a
        aux a ((Add i):xs) = aux (a+i) xs
        aux a ((Sub i):xs) = aux (a-i) xs
```

Hint: you will need *two* continuations to make this work.  Please add continuation arguments
at the top level so that we can test them directly.

I.e, your function will look like

```haskell
calc xx init ka ks = ...
```


Place your new `calc` into `src/Lib.hs`.  The type signature is given for your convenience.
It is defined for arbitrary `Num` types, so we can define our own custom type `PosInt` that
throws an error if you subtract from it and it goes negative.  See the tests for details.
