{-# LANGUAGE ExistentialQuantification #-}
module LispFunc where

import Control.Monad.Except --throwError

import LispData
import Debug.Trace

-- implements the lisp car(head) function
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

--implements the lisp cdr(tail) function
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

unpackBool' :: ThrowsError LispVal -> Bool
unpackBool' (Right (Bool bool))= bool

boolean :: [LispVal] -> ThrowsError LispVal
boolean (Bool _ : [] ) = return $ Bool True
boolean (Bool _ : tail') = return $ Bool (unpackBool' (boolean tail'))
boolean _ = return $ Bool False

---------------------------------------NUMBER FUNCTIONS---------------------------------------------
integer :: [LispVal] -> ThrowsError LispVal
integer (Integer _ : []) = return $ Bool True
integer (Integer _ : xs) = return $ Bool $ unpackBool' $ integer xs
integer _               = return $ Bool False

string :: [LispVal] -> ThrowsError LispVal
string (String _ : []) = return $ Bool True
string (String _ : tail') = return $ Bool (unpackBool' (string tail'))
string _ = return $ Bool False

list :: [LispVal] -> ThrowsError LispVal
list (List _ : []) = return $ Bool True
list (List _ : tail') = return $ Bool (unpackBool' (list tail'))
list _ = return $ Bool False

symbol' :: [LispVal] -> ThrowsError LispVal
--TODO Check this one because its badly coded
symbol' ( val : []) = case val of
                        Atom _-> return $ Bool True
                        otherwise -> return $ Bool False
symbol' ( val : tail') = case val of
                           Atom _-> return $ Bool (unpackBool' (symbol' tail'))
                           otherwise -> return $ Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- "cast" to number and apply the operator
numericBinop op params        = mapM unpackNum params >>= return . Integer . foldl1 op


minus :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
minus op           []  = throwError $ NumArgs 2 []
minus op (x:[]) =  (return . Integer) =<< (fmap negate (unpackNum x))
-- "cast" to number and apply the operator
minus op params        = mapM unpackNum params >>= return . Integer . foldl1 op

-- applies the correct unpacker for the two arguments of a boolean binary operation
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 -- must provide exactly two arguments
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0 --unpack 1st arg
                                     right <- unpacker $ args !! 1 --unpack 2nd arg
                                     return $ Bool $ left `op` right --apply operation


-- applies the correct unpacker for the two arguments of a boolean binary operation
{-boolNop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolNop unpacker op (x:[])= return $ x
boolNop unpacker op (x:y:[])= do left <- unpacker $ x
                                 right <- unpacker $ y
                                 return $ Bool $ left `op` right
boolNop unpacker op (x:y:xs)= do rest <- (boolNop unpacker op xs)
                                 rest' <-  (trace $ "test" ++ show rest) (unpacker rest)
                                 left <-   (unpacker x)
                                 right <- unpacker y
                                 if left `op` right then
                                   return $ Bool $ left  `op` rest'
                                 else
                                   return $ Bool $ False-}

-- conversion functions from lisp vals to haskell val
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Integer s) = return $ show s --Type casting(weak typing)
unpackStr (Bool s)   = return $ show s --Type casting(weak typing)
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Integer n) = return n
-- if the val is a string try to convert it to a number(weak typing)
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
-- singleton list can be converted to numbers, if the val in the list is convertible to number
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

--numBoolNop  = boolNop unpackNum
numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


-- implements the lisp cons(concatination) functionn
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Integer arg1), (Integer arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)


-- ignores type tags, e.g. equal? 2 "2" = #t but eqv? 2 "2" = #f
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



-- Map of all primitive functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", minus (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("modulo", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              --("complex?", numericBinop rem),
              --("real?", numericBinop rem),
              --("rational?", numericBinop rem),
              --("integer?", numericBinop rem),
              --("exact?", numericBinop rem),
              --("inexact?", numericBinop rem),
              --("zero?", numericBinop rem),
              --("positive?", numericBinop rem),
              --("negative?", numericBinop rem),
              --("odd?", numericBinop rem),
              --("even?", numericBinop rem),
              ("integer?", integer),
              ("boolean?", boolean),
              ("list?", numericBinop (+)),
              --("pair?", numericBinop (+)),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", symbol'),
              ("string?", LispFunc.string)]
