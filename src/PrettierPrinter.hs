module PrettierPrinter where

import Common
import Data.List
import qualified Data.Map               as M

type Env = (M.Map Nombre (Lista -> Lista), (M.Map Nombre [Integer]))

instance Show Lista where
  show (ListaNat ls) = ""
  show (Sl ls)= "Si"
  show (Sr ls)= "Sd"
  show (Dl ls)= "Di"
  show (Dr ls)= "Dd"
  show (Ol ls)= "Oi"
  show (Or ls)= "Od"
  show (Rep fun ls)= "<" ++ showFun fun ++ ">"
  show (Variable a) = "Var " ++ a
  show (Funcion a ls) = a

showFun :: (Lista -> Lista) -> String
showFun fun = intercalate " " (map (\x-> show x) (showFun' (fun (ListaNat []))))

showFun' :: Lista -> [Lista]
showFun' (ListaNat ls) = []
showFun' (Sl ls) = showFun' ls ++ [Sl (ListaNat [])]
showFun' (Sr ls) = showFun' ls ++ [Sr (ListaNat [])]
showFun' (Dl ls) = showFun' ls ++ [Dl (ListaNat [])]
showFun' (Dr ls) = showFun' ls ++ [Dr (ListaNat [])]
showFun' (Ol ls) = showFun' ls ++ [Ol (ListaNat [])]
showFun' (Or ls) = showFun' ls ++ [Or (ListaNat [])]
showFun' (Rep fun ls)= showFun' ls ++ [(Rep fun (ListaNat []))]
showFun' (Variable a) = [(Variable a)]
showFun' (Funcion a ls) = showFun' ls ++ [(Funcion a (ListaNat []))]

showIntegerList :: [Integer] -> String
showIntegerList s = "[" ++ intercalate ", " (map (\x-> show x) s) ++ "]"

printError :: Error -> String
printError OperOverEmpty     = "Error: Funcion de lista aplicada sobre lista vacia (fuera de dominio de funcion)"
printError (UndefVar v)      = "Warning: Variable " ++ v ++ " indefinida"
printError (UndefFun v)      = "Warning: Funcion " ++ v ++ " indefinida"
printError (UndefFunOrVar v) = "Warning: No se encontró funcion o variable definida: " ++ v
printError OperOutOfDomain   = "No deberias haber llegado aca"


printFunVar :: PrintDef -> String
printFunVar (OnlyFun fun) = showFun fun
printFunVar (OnlyVar var) = showIntegerList var
printFunVar (FunAndVar (fun, var)) = (showFun fun) ++ "\n" ++ (showIntegerList var)

showEnv :: Env -> IO ()
showEnv env = do putStr ("Funciones: \n" ++ (M.foldrWithKey (\k fun rest -> k ++ ": " ++ (showFun fun) ++ "\n" ++ rest) "" (fst env)) ++ "\n")
                 putStr ("Variables: \n" ++ (M.foldrWithKey (\k var rest -> k ++ ": " ++ (showIntegerList var) ++ "\n" ++ rest) "" (snd env)) ++ "\n")
 