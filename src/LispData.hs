module LispData where


import Data.IORef --IOref
import Control.Monad.Except --ExceptT
import System.IO --Handle
import Number (LispNumber)
import Text.ParserCombinators.Parsec hiding (spaces)

-- Represents our AST, since in Scheme everything is a Val, we don't need a seperate tree representation
--TODO Char, Float, Full-numeric tower
--TODO backquote, vector
data LispVal = Atom String
             | String String -- Stores a Haskell String
             | Bool Bool  -- Stores a Haskell Boolean
             | LispNumber LispNumber
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector [LispVal]
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


-- Type which represents all possible errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where show = showVal
instance Eq LispVal where (==) = equalVal
instance Show LispError where show = showError

--Type alias because all our function now return ThrowsError because they either throw or return valid data
type ThrowsError a = Either LispError a

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError a = ExceptT LispError IO a --TODO Understand ExceptT

equalVal :: LispVal -> LispVal -> Bool
equalVal (Atom a) (Atom b) = a == b
equalVal (LispNumber a) (LispNumber b) = a == b
equalVal (String a) (String b) = a == b
equalVal (Bool a) (Bool b) = a == b
equalVal (List l) (List r) = l == r

showVal :: LispVal -> String
{-showVal (String contents) = "String " ++ (show contents)
showVal (Atom name) = "Atom " ++ (show name)
showVal (LispNumber contents) = "LispNumber" ++ (show contents)
showVal (Bool True) = "Bool True"
showVal (Bool False) = "Bool False"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"
showVal (Vector contents) = "(" ++ unwordsList contents ++ ")"-}

showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (LispNumber contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"
showVal (Vector contents) = "(" ++ unwordsList contents ++ ")" 
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


-- TODO Raff ich immer noch nich ganz
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

-- creates a string from an array of LispVals, it inserts space characters between original strings
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
