module StmtGens where

import Test.QuickCheck hiding (Result)

import qualified Data.HashMap.Strict as H

import ExpGens
import Lib


data StmtEnvPEnvResultUnit =
    StmtEnvPEnvResultUnit
    { stmt :: Stmt
    , env :: Env
    , penv :: PEnv
    , stmtResult :: Result
    }
    deriving(Show)

--- procedure env generator (TODO: make this not trivial)
arbPEnv :: Gen PEnv
arbPEnv = pure H.empty

--- print statements
--- "data Stmt = PrintStmt Exp"
arbPrintStmt :: Gen StmtEnvPEnvResultUnit
arbPrintStmt = do
    palPrime <- elements [13331, 15551, 16661, 19991]
    aPenv <- arbPEnv
    return $ StmtEnvPEnvResultUnit
      { stmt = PrintStmt $ IntExp palPrime
      , env = H.empty
      , penv = aPenv
      , stmtResult = (show palPrime, aPenv, H.empty)
      }

--- ### stmt generators
--- assignment statements
--- "data Stmt = SetStmt String Exp"

arbSetStmt :: Gen StmtEnvPEnvResultUnit
arbSetStmt = do
    var <- arbParam
    aPenv <- arbPEnv
    ExpValUnit iexp ival <- arbIntConstExp
    return $ StmtEnvPEnvResultUnit
      { stmt = SetStmt var iexp
      , env = H.empty
      , penv = aPenv
      , stmtResult = ("", aPenv, H.singleton var ival)
      }

--- sequence statements
--- "data Stmt = SeqStmt [Stmt]"

arbSeqStmt :: Gen StmtEnvPEnvResultUnit
arbSeqStmt = do
    set1 <- arbSetStmt
    set2 <- arbSetStmt
    proc1 <- arbProcStmt
    proc2 <- arbProcStmt
    print1 <- arbPrintStmt
    print2 <- arbPrintStmt
    units <- shuffle [set1,set2,proc1,proc2,print1,print2]
    let
      stmt' = map stmt units
      result = foldl combine ([], H.empty, H.empty) $ map stmtResult units
      combine (s1,p1,e1) (s2,p2,e2) = (s1++s2, H.union p2 p1, H.union e2 e1)
    return $ StmtEnvPEnvResultUnit
      { stmt = SeqStmt stmt'
      , env = H.empty
      , penv = H.empty
      , stmtResult = result
      }

--- if statements
--- "data Stmt = IfStmt Exp Stmt Stmt
arbIfStmt :: Gen StmtEnvPEnvResultUnit
arbIfStmt = do
    ExpValUnit ifexp cval <- arbIfExp
    case ifexp of
      IfExp ce e1 e2 ->
        return $ StmtEnvPEnvResultUnit
          { stmt = IfStmt ce (PrintStmt e1) (PrintStmt e2)
          , env = H.empty
          , penv = H.empty
          , stmtResult = (show cval, H.empty, H.empty)
          }
      _ -> discard

--- procedure statements
--- "data Stmt = ProcedureStmt String [String] Stmt

data ProcedureUnit = ProcedureUnit Stmt ([Val] -> Result)

arbProcedure :: Gen ProcedureUnit
arbProcedure = elements
  [ ProcedureUnit procUpTo funcUpTo ]


arbProcStmt :: Gen StmtEnvPEnvResultUnit
arbProcStmt = do
  ProcedureUnit aProc _ <- arbProcedure
  case aProc of
    ProcedureStmt name _ _ -> 
      return $ StmtEnvPEnvResultUnit
        { stmt = aProc
        , env = H.empty
        , penv = H.empty
        , stmtResult = ("", H.singleton name aProc, H.empty )
        }
    _ -> discard

--- procedure call statement
--- data Stmt = CallStmt String [Exp]
arbCallStmt :: Gen StmtEnvPEnvResultUnit
arbCallStmt = do
  ProcedureUnit aProc aFun <- arbProcedure
  (exp1,val1) <- oneof
    [ ((,) <$> BoolExp <*> BoolVal) <$> arbitrary
    , ((,) <$> IntExp <*> IntVal) <$> choose (0,10)
    ]
  (exp2,val2) <- oneof
    [ ((,) <$> BoolExp <*> BoolVal) <$> arbitrary
    , ((,) <$> IntExp <*> IntVal) <$> choose (10,30)
    ]
  let result@(_, pEnv', _) = aFun [val1,val2]
  case aProc of
    ProcedureStmt name _ _->
      return $ StmtEnvPEnvResultUnit
      { stmt = CallStmt name  [exp1,exp2]
      , env = H.empty
      , penv = pEnv'
      , stmtResult = result
      }
    _ -> discard

--- Caution ---
--- semi general Procedure units
funcUpTo (IntVal v1 : IntVal v2 : _)
    = ( concatMap show [v1..v2-1] ++ "True"
      , H.singleton "upTo" procUpTo
      , H.fromList [ ("lower", IntVal $ if v1 < v2 then v2 else v1 )
                   , ("upper", IntVal v2)]
      )
funcUpTo []
    = (show ifExn, H.singleton "upTo" procUpTo, H.empty)
funcUpTo vs
    = ( show ifExn
      , H.singleton "upTo" procUpTo
      , H.fromList $ zip ["lower" , "upper"] vs
      )

procUpTo =
  ProcedureStmt "upTo" ["lower", "upper"]
  (IfStmt (CompOpExp "<" (VarExp "lower") (VarExp "upper"))
    (SeqStmt [ PrintStmt (VarExp "lower")
             , SetStmt "lower" (IntOpExp "+" (VarExp "lower") (IntExp 1))
             , CallStmt "upTo" [VarExp "lower", VarExp "upper"]
             ]
    )
    (PrintStmt (BoolExp True))
  )

