module Eval where

import           Common
import           Monads
import           PrettierPrinter
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Tuple
import           Data.List
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entorno nulo
initEnv :: Env
initEnv = (M.empty, M.empty)

newtype StateError a = StateError { runStateError :: Env -> (Either Error a, Env) }

instance Monad StateError where
  return x = StateError (\s -> (Right x, s))
  m >>= f  = StateError (\s ->
                    let (ev, s') = runStateError m s
                    in case ev of
                      Left e  -> (Left e, s')
                      Right v -> runStateError (f v) s')

instance MonadError StateError where
  throw e = StateError (\s -> (Left e, s))

instance MonadState StateError where
  lookforVar v = StateError (\s ->
                let x = M.lookup v (snd s)
                in case x of
                  Nothing      -> (Left (UndefVar v), s)
                  Just varList -> (Right varList, s))
  lookforFun v = StateError (\s ->
                let x = M.lookup v (fst s)
                in case x of
                  Nothing      -> (Left (UndefFun v), s)
                  Just funList -> (Right funList, s))
  lookfor v = StateError (\s ->
                let x = M.lookup v (fst s)
                in case x of
                  Nothing      -> case M.lookup v (snd s) of
                                      Nothing      -> (Left (UndefFunOrVar v), s)
                                      Just varList -> (Right (OnlyVar varList), s)
                  Just funList -> case M.lookup v (snd s) of
                                      Nothing      -> (Right (OnlyFun funList), s)
                                      Just varList -> (Right (FunAndVar (funList, varList)), s))
  updateVar v i = StateError (\s -> (Right (), ((fst s), M.insert v i (snd s))))
  updateFun v i = StateError (\s -> (Right (), (M.insert v i (fst s), (snd s))))

instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

eval :: Comm  -> Env -> (Either Error String, Env)
eval comm env = runStateError (evalComm comm) env

evalComm :: (MonadState m, MonadError m) => Comm -> m String
evalComm (Fun name decl) = do updateFun name decl
                              return ""
evalComm (Var name decl) = do lsE <- evalLista decl
                              updateVar name lsE
                              return ""
evalComm (Print name)    = do res <- lookfor name
                              return (showFunVar res)
-- Nunca se deberia llegar a evaluarse un comando que no sea esos 3.
evalComm _    = do return ""                              

evalLista :: (MonadState m, MonadError m) => Lista -> m [Integer]
evalLista (ListaNat ls)     = do return ls
evalLista (Ol ls)           = do lsE <- evalLista ls
                                 return (0 : lsE)
evalLista (Or ls)           = do lsE <- evalLista ls
                                 return (lsE ++ [0])
evalLista (Sl ls)           = do lsE <- evalLista ls
                                 if (lsE == []) then throw OperOverEmpty
                                 else return (((head lsE) + 1) : (tail lsE))
evalLista (Sr ls)           = do lsE <- evalLista ls
                                 if (lsE == []) then throw OperOverEmpty
                                 else return (init lsE ++ [last lsE + 1]) 
evalLista (Dl ls)           = do lsE <- evalLista ls
                                 if (lsE == []) then throw OperOverEmpty
                                 else return (tail lsE)
evalLista (Dr ls)           = do lsE <- evalLista ls
                                 if (lsE == []) then throw OperOverEmpty
                                 else return (init lsE)
evalLista (Int ls)          = evalLista (Mr (Dl (Dr (Rep (\x ->  (Ml (Ml (Sl (Mr (Mr (Sl x))))))) (Ol (Ml (Ol (Mr ls))))))))
evalLista (Rep fun ls)      = do lsE <- evalLista ls
                                 if length lsE < 2 then throw RepOutDomain
                                 else if ((last lsE) == (head lsE)) then return lsE
                                        else (evalLista (Rep (fun) (fun (ListaNat lsE))))
-- Funciones de lista derivadas de las atomicas. Se realiza traduccion al evaluar.
evalLista (Ml ls)           = evalLista (Dr ((Rep (\x -> Sl x) (Ol ls))))
evalLista (Mr ls)           = evalLista (Dl ((Rep (\x -> Sr x) (Or ls))))
evalLista (DDl ls)          = evalLista (Ml ((Rep (\x -> Sr x) (Or ls))))
evalLista (DDr ls)          = evalLista (Mr ((Rep (\x -> Sl x) (Ol ls))))
evalLista (Funcion name ls) = do fun <- lookforFun name 
                                 (evalLista (fun ls))
evalLista (Variable name)   = lookforVar name