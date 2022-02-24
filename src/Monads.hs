module Monads where

import           Common

-- Clase para representar mónadas con estado de variables

class Monad m => MonadState m where
    -- Busca una variable
    lookforVar :: Nombre -> m [Integer]
    -- Busca una funcion
    lookforFun :: Nombre -> m (Lista -> Lista)
    -- Busca una variable o funcion con el nombre dado
    lookfor :: Nombre -> m PrintDef
    -- Cambia el valor de una variable o funcion
    updateVar :: Nombre -> [Integer] -> m ()
    updateFun :: Nombre -> (Lista -> Lista) -> m ()

-- Clase para representar mónadas que lanzan errores de evaluacion

class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a

