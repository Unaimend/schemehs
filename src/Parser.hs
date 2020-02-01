module Parser where

import Control.Monad

import Text.ParserCombinators.Parsec hiding (spaces)
import Debug.Trace
import LispData
--TODO Learn monads again and check what mapM is doing

-- Recognizes if a character is a valid scheme symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Skips one or more spaces
spaces1 :: Parser ()
spaces1 = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'
  x <- oneOf "\"tn\\"
  case x of
    't' -> return '\t'
    'n' -> return '\n'
    'r' -> return '\r'
    '\\' -> return '\\'
    _ -> return x

-- Parses a string which starts with a " and ends with a"
-- TODO \\t \\n \\r \\ \"
parseString :: Parser LispVal
parseString = do
  _ <- char '"' --Starts with a "
  -- Stops at "
  x <- many $ escapedChars <|> noneOf("\"")
  _ <- char '"' --ends with a "
  return $ String x

-- Parses a symbol
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol --First char must be a letter or a sybmol
  --the following chars must be one of letter, digit or symbol
  rest <- {-trace  (show first)-} (many (letter <|> digit <|> symbol))
  let atom = first:rest
  --catch special atoms
  case atom of "#t" -> return $ Bool True
               "#f" -> return $ Bool False
               ('-':x:xs)   -> case x of
                   ' ' -> (return $ Atom atom)
                   _ -> return $ (LispNumber . Integer . read) atom
               _    -> {-trace ("attom"++ show atom)-} (return $ Atom atom)

parseInteger :: Parser LispVal
parseInteger = choice [parseNeg, parsePos]

parseNeg = do sign <- char '-'
              int <- many1 digit
              return $ (LispNumber . Integer . read) int

parsePos = do int <- many1 digit
              return $ (LispNumber . Integer . read) int


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
         <|> parseInteger -- etc
         <|> parseQuoted
         <|> do _ <- char '('
                -- parses a normal list until it encounter a dot, at which point it will go back and sstart to parse
                -- a dotted list
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x


