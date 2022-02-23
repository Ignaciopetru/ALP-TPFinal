module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )
import qualified Data.Map.Strict               as M


import           Common
import           Parse
import           Eval
import           PrettierPrinter

---------------------
--- Interpreter
---------------------

parseComm :: String -> Comm
parseComm contents = func $ lexerComm contents


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
                        x    -> do case (eval x env) of
                                    (Left err, env') -> do outputStrLn (printError err)
                                                           loop env'
                                    (Right s, env') ->  do case s of
                                                              "" -> do loop env'
                                                              ss -> do outputStrLn (ss)
                                                                       loop env'

cleanLine :: String -> String
cleanLine line = if last line == '\n' || last line == '\r' then init line else line

readFromFile :: [String] -> Env -> IO ()
readFromFile []     env = do putStr "Archivo parseado correctamente\n"
                             showEnv env
readFromFile (x:xs) env = case x of
                          ""   -> readFromFile xs env
                          -- Se elimina el \n, por eso init
                          line -> do case parseComm (cleanLine line) of
                                       Exit -> return ()
                                       x    -> do case (eval x env) of
                                                   (Left err, env') -> do putStr (printError err)
                                                   (Right s, env') ->  do case s of
                                                                             "" -> readFromFile xs env'
                                                                             ss -> do putStr (ss)
                                                                                      readFromFile xs env'