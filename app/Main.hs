module Main where
import Interpreter
import System.Environment

import Control.Monad
import System.IO
import LispData
import System.Console.Readline


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = do
    flushStr prompt
    maybeLine <- readline "Lisp>>>"
    case maybeLine of
      Nothing     -> return "" -- EOF / control-d
      Just "exit" -> return ""
      Just line -> do addHistory line
                      return line

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
        >>= hPutStrLn stderr


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action



main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
