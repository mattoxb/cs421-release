{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Common where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.List ((\\), intercalate, union)
import qualified Data.Map.Strict as H (Map, (!), delete, insert, lookup, union, empty, singleton, fromList, toList, elems, keys)

{- language definition -}

data Const 
  = IntConst Int
  | BoolConst Bool
  | StringConst String
  | NilConst
  | UnitConst

data Monop = HdOp | TlOp | PrintOp | IntNegOp | FstOp | SndOp | NotOp

data Binop = IntPlusOp | IntMinusOp | IntTimesOp | IntDivOp
  | ConcatOp | ConsOp | CommaOp | EqOp | GreaterOp

data Exp 
  = ConstExp Const
  | VarExp String
  | MonOpExp Monop Exp
  | BinOpExp Binop Exp Exp
  | IfExp Exp Exp Exp
  | AppExp Exp Exp
  | FunExp String Exp
  | LetExp String Exp Exp
  | LetRecExp String String Exp Exp

data Dec
  = AnonDec Exp
  | LetDec String Exp
  | LetRec String String Exp

instance Show Const where
  show (IntConst i) = show i
  show (BoolConst b) = if b then "true" else "false"
  show (StringConst s) = "\"" ++ s ++ "\""
  show NilConst = "[]"
  show UnitConst = "()"

instance Show Monop where
  show HdOp = "hd"
  show TlOp = "tl"
  show PrintOp = "print"
  show IntNegOp = "~"
  show FstOp = "fst"
  show SndOp = "snd"
  show NotOp = "not"

instance Show Binop where
  show IntPlusOp = "+"
  show IntMinusOp = "-"
  show IntTimesOp = "*"
  show IntDivOp = "/"
  show ConcatOp = "^"
  show ConsOp = "::"
  show CommaOp = ","
  show EqOp = "="
  show GreaterOp = ">"

instance Show Exp where
  show (ConstExp c) = show c
  show (VarExp x) = x
  show (MonOpExp m e') = "(" ++ unwords [show m, show e'] ++ ")"
  show (BinOpExp b e1 e2) =
    "(" ++ unwords [show e1, show b, show e2] ++ ")"
  show (IfExp e1 e2 e3) =
    "(" ++ unwords ["if", show e1, "then", show e2, "else", show e3] ++ ")"
  show (AppExp e1 e2) =
    "(" ++ unwords [show e1, show e2] ++ ")"
  show (FunExp x e') =
    "(" ++ unwords ["fun", x, "->", show e'] ++ ")"
  show (LetExp x e1 e2) =
    "(" ++ unwords ["let", x, "=", show e1, "in", show e2] ++ ")"
  show (LetRecExp f x e1 e2) =
    "(" ++ unwords ["let rec", f, x, "=", show e1, "in", show e2] ++ ")"

instance Show Dec where
  show (AnonDec e') = show e'
  show (LetDec x e') = unwords ["let", x, "=", show e']
  show (LetRec f x e') = unwords ["let rec", f, x, "=", show e']

{- type system definition -}

-- | A type variable
type VarId = Int

-- | Monomorphic types
data MonoTy = TyVar VarId | TyConst String [MonoTy] deriving Eq
-- | Polymorphic types
data PolyTy = Forall [VarId] MonoTy

intTy, boolTy, stringTy, unitTy :: MonoTy
intTy    = TyConst "int" []
boolTy   = TyConst "bool" []
stringTy = TyConst "string" []
unitTy   = TyConst "unit" []

-- | Given 'a, construct the type 'a list.
listTy :: MonoTy -> MonoTy
listTy tau = TyConst "list" [tau]

-- | Given 'a and 'b, construct the type ('a * 'b).
pairTy :: MonoTy -> MonoTy -> MonoTy
pairTy t1 t2 = TyConst "pair" [t1, t2]

-- | Given an argument type 'a and a result type 'b, construct
-- the function type 'a -> 'b.
funTy :: MonoTy -> MonoTy -> MonoTy
funTy t1 t2  = TyConst "->" [t1, t2]

-- | Typing environments (Gamma) map names to polytypes.
-- If you want to put a monotype in the environment, you need
-- to either (1) generalize it or (2) wrap it with Forall []
-- so that none of its type variables are quantified.
type TypeEnv = H.Map String PolyTy

{- mono type print functions -}

-- | A "canonical renaming" of type variables. Variables get renamed from
-- 0, in order by first appearance in monotypes from the left.
type CanonEnv = H.Map VarId Int

-- | Get a name for a renamed type variable. If i >= 26, output will be weird.
tVarName :: Int -> String
tVarName i = ['\'', chr (ord 'a' + i)]

-- | takes a group of MonoTys and creates a canonical renaming for all variables
-- appearing in any of them.
canonize :: (Functor f, Foldable f) => f MonoTy -> CanonEnv
canonize taus = H.fromList $ zip allFreeVars [0..]
  where allFreeVars = unions $ fmap freeVars taus

instance Show MonoTy where
  show tau = showMonoTyWith (canonize [tau]) tau

instance Show PolyTy where
  show (Forall [] tau) = show tau
  show (Forall qVars tau) =
    -- lookup each qVar in cenv, get the tVarName, and stick "forall" in front
    unwords ("forall" : map (tVarName . (cenv H.!)) qVars) 
    ++ ". " ++ showMonoTyWith cenv tau
    where cenv = canonize [tau]

-- | Show a MonoTy, but using a given CanonEnv.
showMonoTyWith :: CanonEnv -> MonoTy -> String
showMonoTyWith env = go
  where
    go (TyVar i) = case H.lookup i env of
      Nothing -> show i
      Just x  -> tVarName x
    go (TyConst "->" [tau1, tau2]) =
      "(" ++ unwords [go tau1, "->", go tau2] ++ ")"
    go (TyConst "pair" [tau1, tau2]) =
      "(" ++ unwords [go tau1, "*", go tau2] ++ ")"
    go (TyConst "list" [tau']) =
      go tau' ++ " list"
    -- this case shouldn't arise since we don't have user-defined types, but the
    -- definition would look like this.
    go (TyConst c ts) = unwords (map go ts ++ [c])

  {- error definitions -}

data TypeError
  = InfiniteType VarId MonoTy -- ^ Occurs check failure
  | Can'tMatch MonoTy MonoTy  -- ^ type constructors in constraint don't match
  | LookupError String        -- ^ variable name not in the TypeEnv

instance Show TypeError where
  show (InfiniteType a tau) = 
    unwords ["Can't construct infinite type", aName, "~", showTau]
    where canonEnv = canonize [tau] -- a is always a free variable of tau
          aName   = showMonoTyWith canonEnv (TyVar a)
          showTau = showMonoTyWith canonEnv tau
  show (Can'tMatch t1 t2) = 
    unwords ["Can't solve constraint:", showType t1, "~", showType t2]
    where canonEnv = canonize [t1, t2]
          showType = showMonoTyWith canonEnv
  show (LookupError x) = "Failed lookup of variable: " ++ x

{- type inference monad -}

-- | We're using a library called 'mtl' to define this monad.
-- mtl stands for "Monad Transformer Library."
--
-- Monads allow us to abstract sequencing operations for different
-- "effects" that we might want in our code, like state, throwing errors,
-- having access to a read-only environment, keeping a growing "log" of
-- of things that have happened, etc.
--
-- In our case, we want three effects, so we make a "stack" of 3 monads.
-- 
-- The outermost is WriterT (T stands for Transformer, again). The
-- "writer" effect keeps track of a growing "log" behind the scenes,
-- making sure that logs produced by each action are accumulated
-- with logs produced by previous actions. We'll use a writer effect to
-- manage our growing list of constraints.
-- 
-- The next one down is ExceptT. The "except" effect, unsurprisingly, handles
-- exceptions.
--
-- Finally, we have State. It's State, instead of StateT, because it is on
-- the bottom of the stack (it isn't transforming anything). We'll use the
-- state effect to keep a counter for generating fresh type variables.
type Infer a = WriterT [Constraint] 
                (ExceptT TypeError 
                (State FVCounter)) a

-- | The "Free Variable Counter" for generating fresh names
type FVCounter = Int
-- | tau1 :~: tau2 demonstrates the constraint that tau1 ~ tau2.
data Constraint = MonoTy :~: MonoTy

instance Show Constraint where
  show (tau1 :~: tau2) = unwords [t1, "~", t2]
    where cenv = canonize [tau1, tau2]
          [t1,t2] = map (showMonoTyWith cenv) [tau1, tau2]
instance Type Constraint where
  apply subst (tau1 :~: tau2) = apply subst tau1 :~: apply subst tau2
  freeVars (tau1 :~: tau2) = freeVars tau1 `union` freeVars tau2

-- | Run an Infer "action" to obtain either an error or the result, giving
-- an initial value for the counter.
--
-- This function is defined for the test suite; we usually want to use runInfer
-- instead, which always gives the initial value 0.
runInferFrom :: Int -> Infer a -> Either TypeError a
runInferFrom n m = case evalState (runExceptT (runWriterT m)) n of
  Left e -> Left e
  Right (a, w) -> Right a

-- | Run an Infer "action" to obtain either an error or the result.
--
-- If you're debugging and you also want to see the generated constraint set,
-- you can use @runInfer $ listen m@ instead of @runInfer m@.
runInfer :: Infer a -> Either TypeError a
runInfer = runInferFrom 0

-- | Returns a VarId that has not been returned by 'fresh' before, and won't
-- be returned by 'fresh' again.
fresh :: Infer VarId
fresh = do
  c <- get
  put $! c + 1 --  $! is like $, but in this case it solves a space leak
  return c

-- | Returns (TyVar n), where n is a fresh VarId. See 'fresh'.
freshTau :: Infer MonoTy
freshTau = TyVar <$> fresh

-- | @constrain s t@ adds a new constraint, s ~ t, to the constraint set.
constrain :: MonoTy -> MonoTy -> Infer ()
constrain tau1 tau2 = tell [tau1 :~: tau2]

{- substitution definitions -}

-- | Substitutions map type variables to monotypes.
type Substitution = H.Map VarId MonoTy

-- | Represents (Haskell) types which contain (PicoML) types.
-- PicoML types can have substitutions applied to them, and they
-- also have a set of free variables.
class Type t where
  apply :: Substitution -> t -> t
  freeVars :: t -> [VarId]

instance Type MonoTy where
  apply subst (TyVar i) = case H.lookup i subst of
    Nothing  -> TyVar i 
    Just tau -> tau
  apply subst (TyConst c tauList) = TyConst c $ map (apply subst) tauList

  freeVars (TyVar i) = [i]
  freeVars (TyConst _ taus) = unions $ map freeVars taus

instance Type PolyTy where
  apply subst (Forall qVars tau) = Forall qVars $ apply subst' tau
    where subst' = foldr H.delete subst qVars
  
  freeVars (Forall qVars tau) = freeVars tau \\ qVars

instance Type TypeEnv where
  apply subst tEnv = apply subst <$> tEnv
  freeVars = unions . fmap freeVars

unions :: Eq a => Foldable f => f [a] -> [a]
unions = foldl union []

-- | The empty substitution
substEmpty :: Substitution
substEmpty = H.empty

-- | Creates a singleton substitution; substInit n tau = {n -> tau}.
substInit :: VarId -> MonoTy -> Substitution
substInit = H.singleton

-- | Compose two substitutions. Note that this is more complicated
-- than just taking the union; we have to make sure that mappings in the
-- right substitution are updated by those in the left one.
substCompose :: Substitution -> Substitution -> Substitution
substCompose s1 s2 = s1 `H.union` fmap (apply s1) s2

-- Can't give a Show instance because Substitution = H.Map VarId MonoTy
-- and there is already an instance (Show k, Show v) => Show (H.Map k v)
showSubstitution :: Substitution -> String
showSubstitution subst = "{" ++ intercalate ", " mappings ++ "}"
  where cenv = H.empty -- canonize $ H.elems subst ++ map TyVar (H.keys subst)
                       -- use the above defn. instead of H.empty if you want to
                       -- print a substitution for debugging purposes,
                       -- but it makes the tests angry.
                       -- We could change the tests to match, but we think the
                       -- tests cases are easier to read with integer VarIds,
                       -- so that the outputs match the inputs.
        varidName varid = case H.lookup varid cenv of
          Nothing -> show varid
          Just rn -> tVarName rn
        showMapping (varid, tau) = unwords
          [varidName varid, "->", showMonoTyWith cenv tau]
        -- H.toList is defined as toList = toAscList, so they are sorted
        mappings = map showMapping $ H.toList subst

{- auxiliary functions for type inferencing -}

-- | Get the (polytype) signature of a constant.
-- All type variables are quantified.
constTySig :: Const -> PolyTy
constTySig IntConst{}    = Forall [] intTy
constTySig BoolConst{}   = Forall [] boolTy
constTySig StringConst{} = Forall [] stringTy
constTySig UnitConst     = Forall [] unitTy
constTySig NilConst      = Forall [0] $ listTy $ TyVar 0

-- | Get the (polytype) signature of a monop.
-- All type variables are quantified.
monopTySig :: Monop -> PolyTy
monopTySig HdOp     = Forall [0]   $ funTy (listTy (TyVar 0)) (TyVar 0)
monopTySig TlOp     = Forall [0]   $ funTy lTau lTau where lTau = listTy (TyVar 0)
monopTySig PrintOp  = Forall []    $ funTy stringTy unitTy
monopTySig IntNegOp = Forall []    $ funTy intTy intTy
monopTySig FstOp    = Forall [0,1] $ funTy (pairTy (TyVar 0) (TyVar 1)) (TyVar 0)
monopTySig SndOp    = Forall [0,1] $ funTy (pairTy (TyVar 0) (TyVar 1)) (TyVar 1)
monopTySig NotOp    = Forall []    $ funTy boolTy boolTy

-- | Get the (polytype) signature of a binop.
-- All type variables are quantified.
binopTySig :: Binop -> PolyTy
binopTySig IntPlusOp  = Forall [] intBinopTy
binopTySig IntMinusOp = Forall [] intBinopTy
binopTySig IntTimesOp = Forall [] intBinopTy
binopTySig IntDivOp   = Forall [] intBinopTy
binopTySig ConcatOp   = Forall [] $ binopFunTy stringTy stringTy stringTy
binopTySig ConsOp     = Forall [0] $ binopFunTy (TyVar 0) lTau lTau
  where lTau = listTy (TyVar 0)

binopTySig CommaOp    = Forall [0,1] $ binopFunTy a b (pairTy a b)
  where a = TyVar 0; b = TyVar 1

binopTySig EqOp       = Forall [0] $ binopFunTy (TyVar 0) (TyVar 0) boolTy
binopTySig GreaterOp  = Forall []  $ binopFunTy intTy intTy boolTy

-- If we wanted, we could write a function to make function types with any
-- number of arguments. How might you do that?
-- Hint: try using foldr
-- | Construct the type of a function with 2 arguments.
binopFunTy :: MonoTy -> MonoTy -> MonoTy -> MonoTy
binopFunTy arg1 arg2 res = funTy arg1 (funTy arg2 res)

intBinopTy :: MonoTy
intBinopTy = binopFunTy intTy intTy intTy

{- GEN function -}

-- | Turn a monotype into a polytype, quantifying no variables.
polyTyOfMonoTy :: MonoTy -> PolyTy
polyTyOfMonoTy = Forall []

-- | Quantify a monotype, turning it into a polytype by
-- quantifying /all/ free variables.
quantifyMonoTy :: MonoTy -> PolyTy
quantifyMonoTy = gen H.empty

-- | Generalize a monotype in a type environment, quantifying all free
-- variables of the monotype that aren't also free variables of the environment.
gen :: TypeEnv -> MonoTy -> PolyTy
gen env tau = Forall (freeVars tau \\ freeVars env) tau