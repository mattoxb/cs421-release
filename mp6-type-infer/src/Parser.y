{

module Parser where

import Data.Char
import Common

}

%name parse Dec
%name parseExp Exp
%name parseEnv Env
%name parseType PolyTy
%name parseEqList EqList
%tokentype { Token }
%error { parseError }

%token
  true   { TRUE }
  false  { FALSE }
  '['    { LBRAC }
  ']'    { RBRAC }
  '('    { LPAREN }
  ')'    { RPAREN }
  '{'    { LARCHER }
  '}'    { RARCHER }
  hd     { HD }
  tl     { TL }
  print  { PRINT }
  '~'    { NEG }
  fst    { FST }
  snd    { SND }
  not    { NOT }
  '+'    { PLUS }
  '-'    { MINUS }
  '*'    { TIMES }
  '/'    { DIV }
  '^'    { CARAT }
  ','    { COMMA }
  cons   { DCOLON }
  '='    { EQUALS }
  '>'    { GE }
  id     { ID $$ }
  int    { INT $$ }
  str    { STRING $$ }
  if     { IF }
  then   { THEN }
  else   { ELSE }
  fun    { FUN }
  arrow  { ARROW }
  let    { LET }
  in     { IN }
  rec    { REC }
  t_int  { T_INT }
  t_bool { T_BOOL }
  t_str  { T_STRING }
  t_unit { T_UNIT }
  t_list { T_LIST }
  '.'    { DOT }

%%

Dec :: { Dec }
Dec : Exp                                  { AnonDec $1 }
    | let id '=' Exp                       { LetDec $2 $4 }
    | let rec id id '=' Exp                { LetRec $3 $4 $6 }

Exp :: { Exp }
Exp  : RBinExp                             { $1 }

RBinExp :: { Exp }
RBinExp : RCExp RBinop RBinExp             { BinOpExp $2 $1 $3 }
        | LBinExp                          { $1 }

LBinExp :: { Exp }
LBinExp : RCExp LBinop ControlExp          { BinOpExp $2 $1 $3 }
        | ControlExp                       { $1 }

RCExp :: { Exp }
RCExp : RCExp LBinop AtomExp               { BinOpExp $2 $1 $3 }
      | AtomExp                            { $1 }

ControlExp :: { Exp }
ControlExp : fun id arrow Exp              { FunExp $2 $4 }
           | if Exp then Exp else Exp      { IfExp $2 $4 $6 }
           | let id '=' Exp in Exp         { LetExp $2 $4 $6 }
           | let rec id id '=' Exp in Exp  { LetRecExp $3 $4 $6 $8 } 
           | Monop Exp                     { MonOpExp $1 $2 }
           | AppExp                        { $1 }

AppExp :: { Exp }
AppExp : AppExp AtomExp                    { AppExp $1 $2 }
       | AtomExp                           { $1 }

AtomExp :: { Exp }
AtomExp : Const                            { ConstExp $1 }
        | id                               { VarExp $1 }  
        | '(' Exp ')'                      { $2 }  

RBinop :: { Binop }
RBinop : cons      { ConsOp }

LBinop :: { Binop }
LBinop : '+'                               { IntPlusOp }
       | '-'                               { IntMinusOp }
       | '*'                               { IntTimesOp }
       | '/'                               { IntDivOp }
       | '^'                               { ConcatOp }
       | ','                               { CommaOp }
       | '='                               { EqOp }
       | '>'                               { GreaterOp }

Monop :: { Monop }
Monop : hd                                 { HdOp }
      | tl                                 { TlOp }
      | print                              { PrintOp }
      | '~'                                { IntNegOp }
      | fst                                { FstOp }
      | snd                                { SndOp }
      | not                                { NotOp }

Const :: { Const }
Const : int                                { IntConst $1 }
      | true                               { BoolConst True }
      | false                              { BoolConst False }
      | str                                { StringConst $1 }
      | '[' ']'                            { NilConst }
      | '(' ')'                            { UnitConst }

EqList :: { [Constraint] }
EqList : '[' ']'                           { [] }
       | '[' Equity EqNEList ']'           { $2 : reverse $3 }

EqNEList :: { [Constraint] }
EqNEList : EqNEList ',' Equity             { $3 : $1 }
         |                                 { [] }

Equity :: { Constraint }
Equity : MonoTy '~' MonoTy                 { $1 :~: $3 }

Env :: { [(String, PolyTy)] }
Env : '{' '}'                              { [] }
    | '{' Bind NEEnv '}'                   { $2 : reverse $3 }

NEEnv :: { [(String, PolyTy)] }
NEEnv : NEEnv ',' Bind                     { $3 : $1 }
      |                                    { [] }

Bind :: { (String, PolyTy) }
Bind : id arrow PolyTy                     { ($1, $3) }

PolyTy :: { PolyTy }
PolyTy : int '.' PolyTy                    { let Forall l tau = $3 
                                              in Forall ($1:l) tau }
       | MonoTy                            { Forall [] $1 }

MonoTy :: { MonoTy }
MonoTy : LinTy '*' MonoTy                  { pairTy $1 $3 }
       | LinTy arrow MonoTy                { funTy $1 $3 }
       | LinTy                             { $1 }

LinTy :: { MonoTy }
LinTy : LinTy t_list                       { listTy $1 }
      | AtomTy                             { $1 }

AtomTy : t_int                             { intTy }
       | t_bool                            { boolTy }
       | t_str                             { stringTy }
       | t_unit                            { unitTy }
       | int                               { TyVar $1 }
       | '(' MonoTy ')'                    { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token 
  = TRUE | FALSE 
  | LBRAC | RBRAC | LPAREN | RPAREN | LARCHER | RARCHER
  | HD | TL | PRINT | NEG | FST | SND | NOT
  | PLUS | MINUS | TIMES | DIV | CARAT | COMMA | DCOLON
  | EQUALS | GE | ID String | INT Int | STRING String
  | IF | THEN | ELSE | FUN | ARROW | LET | IN | REC
  | T_INT | T_BOOL | T_STRING | T_UNIT | T_LIST | DOT

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexWord (c:cs)
  | isDigit c = lexInt (c:cs)
lexer ('"':cs) = lexStrConst cs ""
lexer ('[':cs) = LBRAC : lexer cs
lexer (']':cs) = RBRAC : lexer cs
lexer ('(':cs) = LPAREN : lexer cs
lexer (')':cs) = RPAREN : lexer cs
lexer ('{':cs) = LARCHER : lexer cs
lexer ('}':cs) = RARCHER : lexer cs
lexer ('~':cs) = NEG : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer ('-':'>':cs) = ARROW : lexer cs
lexer ('-':cs) = MINUS : lexer cs
lexer ('*':cs) = TIMES : lexer cs
lexer ('/':cs) = DIV : lexer cs
lexer ('^':cs) = CARAT : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer (':':':':cs) = DCOLON : lexer cs
lexer ('=':cs) = EQUALS : lexer cs
lexer ('>':cs) = GE : lexer cs
lexer ('.':cs) = DOT : lexer cs
lexer (_:cs)   = lexer cs

lexInt cs =
  let (num, cs') = span isDigit cs
    in INT (read num) : lexer cs'

lexWord cs =
  case span isAlpha cs of
    ("true", cs)   -> TRUE : lexer cs
    ("false", cs)  -> FALSE : lexer cs
    ("hd", cs)     -> HD : lexer cs
    ("tl", cs)     -> TL : lexer cs
    ("print", cs)  -> PRINT : lexer cs
    ("fst", cs)    -> FST : lexer cs
    ("snd", cs)    -> SND : lexer cs
    ("not", cs)    -> NOT : lexer cs
    ("if", cs)     -> IF : lexer cs
    ("then", cs)   -> THEN : lexer cs
    ("else", cs)   -> ELSE : lexer cs
    ("fun", cs)    -> FUN : lexer cs
    ("let", cs)    -> LET : lexer cs
    ("in", cs)     -> IN : lexer cs
    ("rec", cs)    -> REC : lexer cs
    ("int", cs)    -> T_INT : lexer cs
    ("bool", cs)   -> T_BOOL : lexer cs
    ("string", cs) -> T_STRING : lexer cs
    ("unit", cs)   -> T_UNIT : lexer cs
    ("list", cs)   -> T_LIST : lexer cs
    (id, cs)       -> ID id : lexer cs

lexStrConst [] s = [STRING $ reverse s]
lexStrConst ('"':cs) s = STRING (reverse s) : lexer cs
lexStrConst (c:cs) s = lexStrConst cs (c:s)

}