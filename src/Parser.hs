module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String --Ctor which takes a string
             | Bool Bool --Ctor which takes a bool

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

p :: Parser String
p  = do many $ noneOf("\"")

parseString :: Parser LispVal
parseString = do
  _ <- char '"' --Starts with a "
  x <- p
  _ <- char '"' --ends with a "
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol --First char must be a letter or a sybmol
  --the following chars must be one of letter, digit or symbol
  rest <- many (letter <|> digit <|> symbol) 
  let atom = first:rest
  if atom == "#t"
    then return $ Bool True
    else if atom == "#f"
            then return $ Bool False
            else return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo = do
  str <- many1 digit
  let str2 = read str :: Integer
  return $ Number str2

--parseNumberBind :: Parser LispVal
--parseNumberBind = many1 digit >>= (read :: Integer) >>= Number



parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head' <- endBy parseExpr spaces
    tail' <- char '.' >> spaces >> parseExpr
    return $ DottedList head' tail'

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

--EVALUATION
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("modulo", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", numericBinop rem),
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
              --("number?", numericBinop rem),
              --("boolean?", boolean),
              ("list?", numericBinop (+)),
              ("pair?", numericBinop (+)),
              ("symbol?", numericBinop (+))]
              --("string?", Parser.string)]

unpackBool :: LispVal -> Bool
unpackBool (Bool bool) = bool

boolean :: [LispVal] -> LispVal
boolean (Bool _ : [] ) = Bool True
boolean (Bool _ : tail') = Bool (unpackBool (boolean tail'))
boolean _ = Bool False

string :: [LispVal] -> LispVal
string (String _ : []) = Bool True
string (String _ : tail') = Bool (unpackBool (Parser.string tail'))
string _ = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum
