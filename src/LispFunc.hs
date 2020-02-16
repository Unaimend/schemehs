{-# LANGUAGE ExistentialQuantification #-}
module LispFunc where

import Control.Monad.Except --throwError

import LispData
import Debug.Trace
import Number

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
number :: [LispVal] -> ThrowsError LispVal
number (LispNumber _ : []) = return $ Bool True
number (LispNumber _ : xs) = return $ Bool $ unpackBool' $ number xs
number _ = return $ Bool False

integer :: [LispVal] -> ThrowsError LispVal
integer (LispNumber (Integer _) : []) = return $ Bool True
integer (LispNumber (Integer _) : xs) = return $ Bool $ unpackBool' $ integer xs
integer _                             = return $ Bool False

rational :: [LispVal] -> ThrowsError LispVal
rational (LispNumber (Integer _) : [])  = return $ Bool True
rational (LispNumber (Integer _) : xs)  = return $ Bool $ unpackBool' $ rational xs
rational (LispNumber (Rational _) : []) = return $ Bool True
rational (LispNumber (Rational _) : xs) = return $ Bool $ unpackBool' $ rational xs
rational _                              = return $ Bool False


real :: [LispVal] -> ThrowsError LispVal
real (LispNumber (Integer _) : [])  = return $ Bool True
real (LispNumber (Integer _) : xs)  = return $ Bool $ unpackBool' $ real xs
real (LispNumber (Rational _) : []) = return $ Bool True
real (LispNumber (Rational _) : xs) = return $ Bool $ unpackBool' $ real xs
real (LispNumber (Real _) : [])     = return $ Bool True
real (LispNumber (Real _) : xs)     = return $ Bool $ unpackBool' $ real xs
real _                              = return $ Bool False

complex :: [LispVal] -> ThrowsError LispVal
complex (LispNumber (Integer _) : [])  = return $ Bool True
complex (LispNumber (Integer _) : xs)  = return $ Bool $ unpackBool' $ complex xs
complex (LispNumber (Rational _) : []) = return $ Bool True
complex (LispNumber (Rational _) : xs) = return $ Bool $ unpackBool' $ complex xs
complex (LispNumber (Real _) : [])     = return $ Bool True
complex (LispNumber (Real _) : xs)     = return $ Bool $ unpackBool' $ complex xs
complex (LispNumber (Complex _) : [])  = return $ Bool True
complex (LispNumber (Complex _) : xs)  = return $ Bool $ unpackBool' $ complex xs
complex _                              = return $ Bool False

exact :: [LispVal] -> ThrowsError LispVal
exact x = Bool <$> (((||) <$> (join $ unpackBool <$> rational x)) <*> (join $ unpackBool <$> integer x))

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

--numericBinop1 :: (LispNumber -> LispNumber -> LispNumber) -> [LispVal] -> ThrowsError LispVal
numericBinOp1 op           []  = throwError $ NumArgs 2 []
numericBinOp1 op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp1 op params =  mapM unpackNum' params >>= return . LispNumber . foldl1 op
-- "cast" to number and apply the operator

minus :: [LispVal] -> ThrowsError LispVal
minus           []  = throwError $ NumArgs 2 []
minus  (x:[]) =  (return . LispNumber ) =<< (fmap negate (unpackNum' x))
-- "cast" to number and apply the operator
minus  params        = mapM unpackNum' params >>= return . LispNumber . foldl1 (-)

-- applies the correct unpacker for the two arguments of a boolean binary operation
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 -- must provide exactly two arguments
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0 --unpack 1st arg
                                     right <- unpacker $ args !! 1 --unpack 2nd arg
                                     return $ Bool $ left `op` right --apply operation

even' :: [LispVal] -> ThrowsError LispVal
even' (n:[]) = (unpackInt n) >>= (return . Bool . even)
even' (n:xs) =  throwError $ NumArgs 2 (n:xs)

odd' :: [LispVal] -> ThrowsError LispVal
odd' (n:[]) = (unpackInt n) >>= (return . Bool . not . even)
odd' (n:xs) =  throwError $ NumArgs 2 (n:xs)

-- conversion functions from lisp vals to haskell val
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (LispNumber s) = return $ show s --Type casting(weak typing)
unpackStr (Bool s)   = return $ show s --Type casting(weak typing)
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackInt :: LispVal -> ThrowsError Integer
unpackInt (LispNumber (Integer n)) = return n
-- if the val is a string try to convert it to a number(weak typing)
unpackInt (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "integer" $ String n
                             else return $ fst $ parsed !! 0
-- singleton list can be converted to numbers, if the val in the list is convertible to number
--unpackInt (List [(LispNumber (Integer n))]) = return $ Integer $ unpackNum' n
unpackInt notNum     = throwError $ TypeMismatch "UnpackInt: number" notNum

unpackNum' :: LispVal -> ThrowsError LispNumber
unpackNum' (LispNumber (Integer n)) = return $ Integer n
unpackNum' (LispNumber (Rational n)) = return $ Rational n
unpackNum' (LispNumber (Real n)) = return $ Real n
-- if the val is a string try to convert it to a number(weak typing)
{-unpackNum' (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0-}
-- singleton list can be converted to numbers, if the val in the list is convertible to number
unpackNum' (List [n]) = unpackNum' n
unpackNum' notNum     = throwError $ TypeMismatch "number" notNum

{-unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LispNumber (Integer n)) = return n
-- if the val is a string try to convert it to a number(weak typing)
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
-- singleton list can be converted to numbers, if the val in the list is convertible to number
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum-}

--numBoolNop  = boolNop unpackNum
numBoolBinop  = boolBinop unpackNum'
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
eqv [(LispNumber arg1), (LispNumber arg2)]         = return $ Bool $ arg1 == arg2
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


-- TODO Understand this part
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
                         [AnyUnpacker unpackNum', AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Map of all primitive functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp1 (+)),
              ("-", minus ),
              ("*", numericBinOp1 (*)),
              ("/", numericBinOp1 (/)),
              --("modulo", numericBinOp1 mod),
              --("quotient", numericBinOp1 quot),
              --("remainder", numericBinOp1 rem),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("odd?", odd'),
              ("even?", even'),
              ("number?", number),
              ("integer?", integer),
              ("rational?", rational),
              ("real?", real),
              ("complex?", complex),
              ("boolean?", boolean),
              --("list?", numericBinop (+)),
              --("pair?", numericBinop (+)),
              ("=", numBoolBinop (==)),
              ("/=", numBoolBinop (/=)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
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
              ("string?", LispFunc.string),
              --("inexact?", exact),
              ("exact?", exact)]
