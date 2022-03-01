module Main where

import           Control.Monad.Except
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import           System.Environment

import           Common
import           Parse
import           Eval
import           PrettierPrinter

---------------------
--- Interpreter
---------------------

main :: IO ()
main = do args <- getArgs
          if args /= [] then
            do let filename = head args
               putStr ("Se ingreso el siguiente archivo: " ++ filename ++ "\n")
               if isSuffixOf ".frl" filename then
                 do content <- readFile filename
                    let linesOfFile = lines content
                    readFromFile linesOfFile initEnv
               else
                 do putStr "Archivo con formato no compatible (solo archivos .frl).\n"
          else 
            do putStr "No se ingreso archivo, se procede a ejecutar la consola.\n"
               readFromConsol

readFromConsol :: IO ()
readFromConsol = runInputT defaultSettings (loop initEnv)
        where
          loop :: Env -> InputT IO ()
          loop env = 
            do minput <- getInputLine "FRL> "
               case minput of
                 Nothing -> return ()
                 Just "" -> 
                   do loop env
                 Just input ->
                   do case parseComm input of
                        Exit -> return ()
                        (ParseError err) -> do outputStrLn err
                                               loop env
                        x    -> do case (eval x env) of
                                    (Left err, env') -> do outputStrLn (showError err)
                                                           loop env'
                                    (Right s, env') ->  do case s of
                                                              "" -> do loop env'
                                                              ss -> do outputStrLn (ss)
                                                                       loop env'

readFromFile :: [String] -> Env -> IO ()
readFromFile []     env = do putStr "Archivo parseado correctamente\n"
                             showEnv env
readFromFile (x:xs) env = case x of
                          ""   -> readFromFile xs env
                          -- Se elimina el \n, por eso init
                          line -> do case parseComm (cleanLine line) of
                                       Exit -> do showEnv env
                                                  return ()
                                       (ParseError err) -> do putStr err
                                                              return ()
                                       y    -> do case (eval y env) of
                                                   (Left err, _) -> do putStr (showError err)
                                                   (Right s, env') ->  do case s of
                                                                             "" -> readFromFile xs env'
                                                                             _  -> readFromFile xs env'

cleanLine :: String -> String
cleanLine line = if last line == '\n' || last line == '\r' then init line else line

parseComm :: String -> Comm
parseComm contents = case func $ lexerComm contents of
                       Okey ast -> ast
                       Failed err -> ParseError err