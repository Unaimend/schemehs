module Interpreter where 

import Text.ParserCombinators.Parsec hiding (spaces)

import LispFunc
import LispData
import Parser
import Data.IORef
import Control.Monad.Except
import System.IO
import Number

spaces :: Parser ()
spaces = skipMany1 space

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef --get the environment
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings' env = fmap (++ env) (mapM addBinding bindings')
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

eval :: Env -> LispVal -> IOThrowsError LispVal
-- Lift values into ThrowError LispVal because they evaluate to themselves
eval _ val@(String _) = return val
eval _ val@(LispNumber (Integer _)) = return val
eval _ val@(LispNumber (Real _)) = return val
eval _ val@(LispNumber (Complex _)) = return val
eval _ val@(LispNumber (Rational _)) = return val
eval _ val@(Bool _) = return val
eval env (Atom at) =  getVar env at
-- a quoted list should be taken as a literal, without evaluating its content
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval _ (List [Atom "quote", val]) = return val
-- Since everythin in lisp starts with a (, everything will be parsed as list with content
-- so we test against a List with sth. inside
eval env (List [Atom "if", cond, conseq, alt]) =
  --evaluate the condition of the if
     do result <- eval env cond
        case result of
          --evaluate else
             Bool False -> eval env alt
             --evaluate then
             _ -> eval env conseq
-- applies func to all evaluated args
-- TODO Imlement different evaluation orders
eval env (List (Atom "define" : List (Atom var : params') : body')) =
     makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
     makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
     makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
     makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
     makeVarArgs varargs env [] body'
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List [Atom "load", String filename]) = 
     load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
-- Since this is the last pattern it will only match if all other pattern failed which means that we parsed invalid lisp code
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


primitiveBindings :: IO Env
primitiveBindings = ref >>= flip bindVars (map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
           ref = (nullEnv >>= flip bindVars [("PI", ((LispNumber (Real pi))))])


makeFunc :: Monad m =>
                  Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params' body' = return $ Func (map showVal params') varargs body' env

makeNormalFunc :: Env
               -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal
            -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params' varargs body' closure') args =
      if num params' /= num args && varargs == Nothing
         then throwError $ NumArgs (num params') args
         else liftIO (bindVars closure' $ zip params' args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params') args
            num = toInteger . length
            evalBody env = last <$> mapM (eval env) body'
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
apply f _ = throwError $ BadSpecialForm "Missing space before functions application" f

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap List $ load filename



readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
