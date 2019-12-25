module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector [LispVal]
             | Number Integer
             | String String --Ctor which takes a string
             | Bool Bool --deriving(Show) --Ctor which takes a bool

instance Show LispVal where show = showVal





-- Recognizes if a character is a valid scheme symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Skips one or more spaces
spaces1 :: Parser ()
spaces1 = skipMany1 space

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
  case atom of "#t" -> return $ Bool True
               "#f" -> return $ Bool True
               "#"  -> parseVector
               _    -> return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumberDo :: Parser LispVal
parseNumberDo = do
  str <- many1 digit
  let str2 = read str :: Integer
  return $ Number str2

parseVector :: Parser LispVal
parseVector = do
  _ <- char '('
  vec <- sepBy parseExpr spaces1
  _ <- char ')'
  return $ Vector vec

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces1

parseDottedList :: Parser LispVal
parseDottedList = do
    head' <- endBy parseExpr spaces1
    tail' <- char '.' >> spaces1 >> parseExpr
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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"
showVal (Vector contents) = "(" ++ unwordsList contents ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

