{ 
module Parse where

import Common
import Data.Char
import Data.List
}

%name func 
%tokentype { Token } 
%error { parseError }

%token
    Oi          { TOL }
    Od          { TOR }
    Si          { TSL }
    Sd          { TSR }
    Di          { TDL }
    Dd          { TDR }
    '<'         { TRepL }
    '>'         { TRepR }
    '['         { TBracketL }
    ']'         { TBracketR }
    ','         { TComa }
    Nat         { TNat $$ }
    Var         { TVar $$ }
    '='         { TEqual }
    Exit        { TExit }
    Funcion     { TFuncion }
    Variable    { TVariable }
    Print       { TPrint }

%right '<' '>'
%right Oi Od Si Sd Di Dd

%%


comm :: { Comm }
comm : Funcion  Var '=' listaRep { Fun $2 $4 }
comm : Variable Var '=' lista    { Var $2 $4 }
comm : Print Var                 { Print $2 }
comm : Exit                      { Exit }

lista :: { Lista }
lista :  listaNat                { ListaNat $1 }
      |  Oi lista                { Ol $2 }
      |  Od lista                { Or $2 }
      |  Si lista                { Sl $2 }
      |  Sd lista                { Sr $2 }
      |  Di lista                { Dl $2 }
      |  Dd lista                { Dr $2 }
      |  '<' listaRep '>' lista  { Rep (\x -> $2 x ) $4 }
      |  Var lista               { Funcion $1 $2 }
      |  Var                     { Variable $1 }

listaNat :  '[' nats ']'         { $2 }
         |  '[' ']'              { [] } 

nats :: { [Integer] } 
nats : Nat                       { [$1] }
     | Nat ',' nats              { $1 : $3 }

listaRep :: { Funcion }
listaRep :  Oi listaRep          { \x -> Ol ($2 x) }
         |  Od listaRep          { \x -> Or ($2 x) }
         |  Si listaRep          { \x -> Sl ($2 x) }
         |  Sd listaRep          { \x -> Sr ($2 x) }
         |  Di listaRep          { \x -> Dl ($2 x) }
         |  Dd listaRep          { \x -> Dr ($2 x) }
         |  '<' listaRep '>' listaRep { \x -> Rep (\x -> $2 x ) ($4 x) }
         |  Var listaRep         { \x -> (Funcion $1 ($2 x)) }
         |  Oi                   { Ol }
         |  Od                   { Or }
         |  Si                   { Sl }
         |  Sd                   { Sr }
         |  Di                   { Dl }
         |  Dd                   { Dr }
         |  '<' listaRep '>'     { Rep (\x -> $2 x ) }
         |  Var                  { \x -> (Funcion $1 x) }

{
data Token =  TOR
            | TOL
            | TSL
            | TSR
            | TDL
            | TDR
            | TRepL
            | TRepR
            | TBracketR
            | TBracketL
            | TComa
            | TNat Integer
            | TVar String
            | TEqual
            | TExit
            | TPrint
            | TVariable
            | TFuncion

            deriving (Show, Eq)

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseErrorCustom :: String -> a
parseErrorCustom e = error e

-- Main lexer
lexerComm :: String -> [Token]
lexerComm [] = []
lexerComm cs@(c:cc) | isSpace c = lexerComm cc
                    | otherwise = case span isAlphaNum cs of
                                       ("Exit", rest) -> [TExit]
                                       ("Fun", rest) -> TFuncion : lexerFun rest
                                       ("Var", rest) -> TVariable : lexerVar rest
                                       ("Print", rest) -> TPrint : lexerVar rest
                                       (v, rest) -> TVar v : lexerComm rest

-- lexerVar
lexerVar :: String -> [Token]
lexerVar [] = []
lexerVar cs@(c:cc) | isSpace c = lexerVar cc
                   | c == '='  = TEqual : reverseVarList cc
                   | otherwise = case span isAlphaNum cs of
                                       (v, rest) -> TVar v : lexerVar rest
-- lexerFun
lexerFun :: String -> [Token]
lexerFun [] = []
lexerFun cs@(c:cc) | isSpace c = lexerFun cc
                   | c == '='  = TEqual : reverseFunList cc
                   | otherwise = case span isAlphaNum cs of
                                       (v, rest) -> TVar v : lexerFun rest

-- Da vuelta la lista del fst respetando <> y concatena
reversePredicate :: ([Token], [Token]) -> [Token]
reversePredicate (operators, list) = map (\x -> if (x /= TRepL && x /= TRepR) then x else (if x == TRepL then TRepR else TRepL)) (reverse operators) ++ list

-- Al declararse una variable, la lista de elementos sobre los cuales operar o ultima variable sobre la que se opera no deben invertirse.
reverseVarList :: String -> [Token]
reverseVarList x = let tokenList = lexer' x
                  in case (TBracketL `elem` tokenList) of
                          True -> reversePredicate (span (\z -> z /= TBracketL) tokenList)
                          False -> reversePredicate ((init tokenList), [last tokenList])

-- Al ser funcion, todos los operadores tienen que ser invertidos.
reverseFunList :: String -> [Token]
reverseFunList x = reversePredicate (lexer' x, [])

lexer' :: String -> [Token]
lexer' [] = []
lexer' cs@(c:cc) | isSpace c = lexer' cc
                 | c == '['  = TBracketL : lexerListNat cc
                 | otherwise = lexerOper cs

lexerListNat :: String -> [Token]
lexerListNat [] = []
lexerListNat (',':cs) = TComa : lexerListNat cs
lexerListNat ('[':cs) = TBracketL : lexerListNat cs
lexerListNat (']':cs) = TBracketR : lexerListNat cs
lexerListNat (' ':cs) = lexerListNat cs
lexerListNat (c:cs)
             | isDigit c = lexerNat (c:cs)
             | otherwise = parseError []

lexerNat :: String -> [Token]
lexerNat [] = []
lexerNat cs = case span isDigit cs of  
                   (num, rest) -> (TNat (read num :: Integer)) : lexerListNat rest

lexerOper :: String -> [Token]
lexerOper cs = case span isAlphaNum cs of
                    ("Oi", rest) -> TOL : lexer' rest
                    ("Od", rest) -> TOR : lexer' rest
                    ("Si", rest) -> TSL : lexer' rest
                    ("Sd", rest) -> TSR : lexer' rest
                    ("Di", rest) -> TDL : lexer' rest
                    ("Dd", rest) -> TDR : lexer' rest
                    (var, rest)  -> if var == "" then lexerRep cs else (TVar var) : lexer' rest
                    otherwise -> lexerRep cs

lexerRep :: String -> [Token]
lexerRep [] = []
lexerRep cs@(c:cc) | '<' == c = TRepL : lexer' cc
                   | '>' == c = TRepR : lexer' cc
                   | isSpace c = lexer' cs
                   | otherwise = parseErrorCustom ("Caracter no reconocido: " ++ [c])
}
