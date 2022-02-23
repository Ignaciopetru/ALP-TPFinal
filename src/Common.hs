module Common where
import Data.List

data Comm = Fun Nombre Funcion | Var Nombre Lista | Print Nombre | Exit

data Lista = ListaNat [Integer]
            | Sl Lista
            | Sr Lista 
            | Ol Lista 
            | Or Lista 
            | Dl Lista 
            | Dr Lista 
            | Rep Funcion Lista
            | Funcion Nombre Lista
            | Variable Nombre

type Funcion = (Lista -> Lista)

-- Tipo utilizado para devolver del env la funcion / lista asociada a una variable.
data PrintDef = OnlyFun Funcion | OnlyVar [Integer] | FunAndVar (Funcion, [Integer])

data Error = OperOverEmpty | UndefVar Nombre | UndefFunOrVar Nombre | OperOutOfDomain | UndefFun Nombre deriving (Eq, Show)

type Nombre = String
