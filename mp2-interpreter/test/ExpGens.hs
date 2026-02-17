module ExpGens where

import Test.QuickCheck

import qualified Data.HashMap.Strict as H

import Lib


--- Lifting Functions
--- -----------------

--- Input x Result unit test records
data LiftOpUnit lit =
  LiftOpUnit
  { arg1 :: Val
  , arg2 :: Val
  , op   :: Fun (lit, lit) Bool
  , expResult :: Val
  }
  deriving(Show)

arbMostlyBoolVal :: Gen Val
arbMostlyBoolVal =
  frequency [ (9, BoolVal <$> arbitrary), (1, IntVal <$> arbitrary) ]
                                                         
arbLiftBool :: Gen (LiftOpUnit Bool)
arbLiftBool = wrap <$> arbMostlyBoolVal <*> arbMostlyBoolVal <*> arbitrary
  where wrap v1 v2 op = LiftOpUnit v1 v2 op
          $ case (v1,v2) of
              (BoolVal b1, BoolVal b2) -> BoolVal $ applyFun2 op b1  b2
              _                        -> liftExn

arbMostlyIntVal :: Gen Val
arbMostlyIntVal =
  frequency [ (9, IntVal <$> arbitrary), (1, BoolVal <$> arbitrary) ]

arbLiftComp :: Gen (LiftOpUnit Int)
arbLiftComp = wrap <$> arbMostlyIntVal <*> arbMostlyIntVal <*> arbitrary
  where wrap v1 v2 op =  LiftOpUnit v1 v2 op
          $ case (v1,v2) of
              (IntVal i1, IntVal i2) -> BoolVal $ applyFun2 op i1  i2
              _                        -> liftExn

--- eval
--- ----

-- Expression x Value Unit test pairs
data ExpValUnit = ExpValUnit Exp Val
  deriving(Show, Eq) 
                
-- Syntax Op pair Generators
arbIntOp :: Gen (String, Int -> Int -> Int)
arbIntOp = elements $ H.toList intOps

arbBoolOp :: Gen (String, Bool -> Bool -> Bool)
arbBoolOp = elements $ H.toList boolOps
  
arbCompOp :: Gen (String, Int -> Int -> Bool)
arbCompOp = elements $ H.toList compOps


--- ### Constants
arbBoolConstExp = (ExpValUnit <$> BoolExp <*> BoolVal) <$> arbitrary

arbIntConstExp  = (ExpValUnit <$> IntExp <*> IntVal) <$> arbitrary 

arbConstExp :: Gen ExpValUnit
arbConstExp = oneof [ (ExpValUnit <$> IntExp  <*>  IntVal) <$> arbitrary
                    , (ExpValUnit <$> BoolExp <*> BoolVal) <$> arbitrary ] 

-- Exception Value Constants
liftExn = ExnVal "Cannot lift"
divExn  = ExnVal "Division by 0"
varExn  = ExnVal "No match in env"
ifExn   = ExnVal "Condition is not a Bool"
funExn  = ExnVal "Apply to non-closure"
                    
--- ### Variables
arbParamList :: Gen [String]
arbParamList = shuffle $ map (:"") ['a'..'z']

arbParam :: Gen String
arbParam = elements $ map (:"") ['a'..'z']

arbVal :: Gen Val
arbVal = oneof [IntVal <$> arbitrary, BoolVal <$> arbitrary] -- add closures

arbVarExp :: Gen ExpEnvValUnit
arbVarExp = do
    var <- arbParam
    env <- arbEnv
    val <- arbVal
    frequency
        [ ( 4, return $ ExpEnvValUnit (VarExp var) (H.insert var val env) val )
        , ( 1, return $ ExpEnvValUnit (VarExp var) (H.delete var env) varExn )
        ]
  
  
--- ### Arithmetic
arbMostlyIntExp :: Gen ExpValUnit
arbMostlyIntExp = frequency [ (19, (ExpValUnit <$> IntExp <*> IntVal) <$> arbitrary )
                            , (1, (flip ExpValUnit liftExn) <$> (BoolExp <$> arbitrary) ) ]

arbIntOpExp :: Int -> Gen ExpValUnit
arbIntOpExp 0 = arbMostlyIntExp
arbIntOpExp n | n > 0 = do
    ExpValUnit e1 v1 <- choose (0, n-1) >>= arbIntOpExp
    ExpValUnit e2 v2 <- choose (0, n-1) >>= arbIntOpExp
    (opS,op) <- arbIntOp
    let val = case (v1,v2,opS) of
          (_,         IntVal  0,"/") -> divExn
          (IntVal i1, IntVal i2,  _) -> IntVal $ i1 `op` i2
          (_        , _        ,  _) -> liftExn
    return $ ExpValUnit (IntOpExp opS e1 e2) val
                 



--- ### Boolean and Comparison Operators
arbCompOpExp :: Int -> Gen ExpValUnit
arbCompOpExp 0 = arbMostlyIntExp
arbCompOpExp n | n > 0 = do
    ExpValUnit e1 v1 <- choose (0, n-1) >>= arbIntOpExp
    ExpValUnit e2 v2 <- choose (0, n-1) >>= arbIntOpExp
    (opS,op) <- arbCompOp
    let val = case (v1,v2) of
          (IntVal i1, IntVal i2) -> BoolVal $ i1 `op` i2
          (_        , _        ) -> liftExn
    return $ ExpValUnit (CompOpExp opS e1 e2) val
                 

arbMostlyBoolExp :: Gen ExpValUnit
arbMostlyBoolExp = frequency [ (19, (ExpValUnit <$> BoolExp <*> BoolVal) <$> arbitrary )
                             , (1, flip ExpValUnit liftExn <$> (IntExp <$> arbitrary) ) ]
      
arbBoolOpExp :: Int -> Gen ExpValUnit
arbBoolOpExp 0 = arbMostlyBoolExp
arbBoolOpExp n | n > 0 = do
   ExpValUnit e1 v1 <- choose (0, n-1) >>= arbBoolOpExp
   ExpValUnit e2 v2 <- choose (0, n-1) >>= arbBoolOpExp
   (opS,op) <- arbBoolOp
   let val = case (v1,v2) of
         (BoolVal b1, BoolVal b2) -> BoolVal $ b1 `op` b2
         (_        , _          ) -> liftExn
   return $ ExpValUnit (BoolOpExp opS e1 e2) val
                                                           
    
--- ### If Expressions

arbIfExp :: Gen ExpValUnit
arbIfExp = do
     ExpValUnit ce cv <- choose (0, 2) >>= arbBoolOpExp
     ExpValUnit e1 v1 <- arbConstExp
     ExpValUnit e2 v2 <- arbConstExp
     let val = case cv of
           BoolVal b -> if b then v1 else v2
           _         -> ifExn
     return $ ExpValUnit (IfExp ce e1 e2) val




--- ### Functions

-- | Expression Environment Value unit test triples
data ExpEnvValUnit = ExpEnvValUnit Exp Env Val
  deriving(Show, Eq)


arbEnv :: Gen Env
arbEnv = do
  ps <- resize 3 $ listOf arbParam
  vs <- listOf arbVal
  return $ H.fromList $ zip ps vs
  
-- replace with arbitrary expressions
arbFunExp :: Gen ExpEnvValUnit
arbFunExp = do
    ExpValUnit e _ <- arbExp
    ps <- resize 3 $ listOf arbParam
    env <- arbEnv
    return $ ExpEnvValUnit (FunExp ps e) env $  CloVal ps e env
  where
    arbExp = oneof
      [ arbIntOpExp 1, arbCompOpExp 1
      , arbBoolOpExp 1, arbConstExp
      , (\(ExpEnvValUnit e _ v) -> ExpValUnit e v) <$> arbVarExp
      ]

arbEnvList :: Gen [(String, Bool)]
arbEnvList = do
    ps <-  arbParamList
    length <- choose (0,5)
    vs <- listOf arbitrary
    return $ zip (take length ps)  vs
                

arbNestedVarExp :: [(String, Bool)] -> Gen (Exp,Val,[(String,Exp)])
arbNestedVarExp [] = uncurry (,,) <$> arbBoolConstExp' <*> pure []
  where arbBoolConstExp'
          = (\(ExpValUnit e v) -> (e,v)) <$> arbBoolConstExp
                        
arbNestedVarExp ((p,b1):ps) = do
    (opS,op) <- arbBoolOp
    boolUnit <- arbNestedVarExp ps
    case boolUnit of
      (e, BoolVal b2, env) ->
        return ( BoolOpExp opS (VarExp p) e
           , BoolVal (b1 `op` b2)
           , (p, BoolExp b1):env )  
      _ -> discard   

arbAppExp :: Gen ExpEnvValUnit
arbAppExp = do
    env <- arbEnv
    argParam <- arbEnvList
    (body, val, argExpList) <- arbNestedVarExp argParam
    let (ps,expList) = unzip argExpList
    frequency [ (9, return $ ExpEnvValUnit
                    (AppExp (FunExp ps body) expList) env val)
              , (1, return $ ExpEnvValUnit
                    (AppExp body expList) env funExn)
              ]
               

--- ### Let Expressions
arbLetExp :: Gen ExpEnvValUnit
arbLetExp = do
    env <- arbEnv
    argParam <- arbEnvList
    (body, val, argExpList) <- arbNestedVarExp argParam
    return $ ExpEnvValUnit (LetExp argExpList body) env val
