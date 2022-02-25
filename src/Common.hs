module Common where
import Data.List

data Comm = Fun Nombre Funcion 
          | Var Nombre Lista
          | Print Nombre
          | ParseError String
          | Exit

data Lista = ListaNat [Integer]
           | Sl Lista
           | Sr Lista
           | Ol Lista
           | Or Lista
           | Dl Lista
           | Dr Lista
           | Ml Lista
           | Mr Lista
           | DDl Lista
           | DDr Lista
           | Int Lista
           | Rep Funcion Lista
           | Funcion Nombre Lista
           | Variable Nombre

type Nombre = String

type Funcion = (Lista -> Lista)

-- Tipo utilizado para devolver del env la funcion / lista asociada a una variable.
-- Ya que un mismo nombre puede referenciar tanto a una variable como a una funcion.
data PrintDef = OnlyFun Funcion 
              | OnlyVar [Integer] 
              | FunAndVar (Funcion, [Integer])

-- Errores de evaluacion.
data Error = OperOverEmpty | UndefVar Nombre | UndefFunOrVar Nombre | RepOutDomain | UndefFun Nombre deriving (Eq, Show)

-- Errores de parseo.
-- Monada de control de errores para Happy (ver documentacion happy).
data E a = Okey a | Failed String

thenE :: E a -> (a -> E b) -> E b
thenE m k =
    case m of
      Okey a -> k a
      Failed e -> Failed e

returnE :: a -> E a
returnE a = Okey a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
              Okey a -> Okey a
              Failed e -> k e
