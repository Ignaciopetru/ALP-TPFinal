module PrettierPrinter where

import Common
import Data.List
import qualified Data.Map               as M

-- Definido en este punto para poder imprimirlo (es importado en eval.hs).
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

-- Se evalua la funcion en una lista para poder recorrerla e impimirla.
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

showError :: Error -> String
showError OperOverEmpty     = "\nError: Funcion de lista aplicada sobre lista vacia (fuera de dominio de funcion)\n"
showError (UndefVar v)      = "\nWarning: Variable " ++ v ++ " indefinida\n"
showError (UndefFun v)      = "\nWarning: Funcion " ++ v ++ " indefinida\n"
showError (UndefFunOrVar v) = "\nWarning: No se encontró funcion o variable definida: " ++ v ++ "\n"
showError RepOutDomain   = "\nError: Repetición aplicada a una lista de menos de 2 elementos\n"

showFunVar :: PrintDef -> String
showFunVar (OnlyFun fun) = showFun fun
showFunVar (OnlyVar var) = showIntegerList var
showFunVar (FunAndVar (fun, var)) = (showFun fun) ++ "\n" ++ (showIntegerList var)

showEnv :: Env -> IO ()
showEnv env = do putStr ("Funciones: \n" ++ (M.foldrWithKey (\k fun rest -> k ++ ": " ++ (showFun fun) ++ "\n" ++ rest) "" (fst env)) ++ "\n")
                 putStr ("Variables: \n" ++ (M.foldrWithKey (\k var rest -> k ++ ": " ++ (showIntegerList var) ++ "\n" ++ rest) "" (snd env)) ++ "\n")
