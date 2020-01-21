module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.IORef
import Control.Monad.Except
-- Represents our AST, since in Scheme everything is a Val, we don't need a seperate tree representation
--TODO Char, Float, Full-numeric tower
--TODO backquote, vector



--TODO Learn monads again and check what mapM is doing

-- Type which represents all possible errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

--Type alias because all our function now return ThrowsError because they either throw or return valid data
type ThrowsError a = Either LispError a

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError a = ExceptT LispError IO a
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector [LispVal]
             | Number Integer -- Stores a Haskell Integer
             | String String -- Stores a Haskell String
             | Bool Bool  -- Stores a Haskell Boolean
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
instance Show LispVal where show = showVal


-- Recognizes if a character is a valid scheme symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Skips one or more spaces
spaces1 :: Parser ()
spaces1 = skipMany1 space


-- Parses a string which starts with a " and ends with a"
-- TODO \\t \\n \\r \\ \" 
parseString :: Parser LispVal
parseString = do
  _ <- char '"' --Starts with a "
  -- Stops at "
  x <- many $ noneOf("\"")
  _ <- char '"' --ends with a "
  return $ String x

-- Parses a symbol
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol --First char must be a letter or a sybmol
  --the following chars must be one of letter, digit or symbol
  rest <- many (letter <|> digit <|> symbol) 
  let atom = first:rest
  --catch special atoms
  case atom of "#t" -> return $ Bool True
               "#f" -> return $ Bool True
               --"#"  -> parseVector
               _    -> return $ Atom atom

--TODO Support octal and hexadecimal notation
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseVector :: Parser LispVal
parseVector = do
  _ <- char '('
  vec <- sepBy parseExpr spaces1
  _ <- char ')'
  return $ Vector vec

parseList :: Parser LispVal
-- Parse lispExpr which hare seperated by one or more whitespace
parseList = liftM List $ sepBy parseExpr spaces1

parseDottedList :: Parser LispVal
parseDottedList = do
    -- what?
    head' <- endBy parseExpr spaces1
    -- parses a dot then exactly one space and saves the expression after the space
    tail' <- char '.' >> spaces1 >> parseExpr
    return $ DottedList head' tail'

-- TODO READ r5rs
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom --first try to parse a atom
         <|> parseString -- if this fails try to parse a string
         <|> parseNumber -- etc
         <|> parseQuoted
         <|> do _ <- char '('
                -- parses a normal list until it encounter a dot, at which point it will go back and sstart to parse
                -- a dotted list
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
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

-- creates a string from an array of LispVals, it inserts space characters between original strings
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



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
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
