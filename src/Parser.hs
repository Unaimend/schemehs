module Parser where

import Data.Ratio
import Data.Complex

import Number
import LispData
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Number


import LispData
--TODO Learn monads again and check what mapM is doing

-- Recognizes if a character is a valid scheme symbol
symbol :: Parser Char
symbol = oneOf "-.!#$%&|*+/:<=>?@^_~"
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
  x <- many $ escapedChars <|> noneOf "\""
  _ <- char '"' --ends with a "
  return $ String x

-- Parses a symbol
parseAtom :: Parser LispVal
parseAtom = do
  first <- (letter <|> symbol) <?> "I HATE PARSEC"
  --first <- choice symlpars
  --the following chars must be one of letter, digit or symbol
  rest <-  many (letter <|> digit <|> symbol)
  let atom = first :rest
  --catch special atoms
  case atom of "#t" -> return $ Bool True
               "#f" -> return $ Bool False
               ('-':x:_)   -> case x of
                   ' ' -> return $ Atom atom
                   _   -> parseNumber
                             --return $ (LispNumber . Integer . read) atom --TODO THIS IS NOT GOOD, try to parse -3o
               _    -> {-trace ("attom"++ show atom)-} (return $ Atom atom)

parseComplex :: Parser LispVal
parseComplex = do
  f <- parseNumber
  s <- char '+'
  n <- parseNumber
  i <- char 'i' 
  let x = case f of
        ((LispNumber (Integer n))) -> (fromIntegral n :: Double)
        ((LispNumber (Rational n))) -> (realToFrac n)
        ((LispNumber (Real n))) -> n
        _ -> 0

  let y = case n of
        ((LispNumber (Integer p))) -> (fromIntegral p :: Double)
        ((LispNumber (Rational p))) -> (realToFrac p)
        ((LispNumber (Real p))) -> p
        _ -> 0
  return $ (LispNumber . Complex) (x :+ y)

parseNegFloat :: Parser LispVal
parseNegFloat = do
  s <-  sign
  beforeDot <- int
  _ <- char '.' <?> "Floating Point Parse Error: expecting ."
  afterDot <- int
  let d = s (read (show beforeDot ++ "." ++ (show afterDot)))
  return $ (LispNumber . Real) d

parseNegRational :: Parser LispVal
parseNegRational = do
  top <- int
  _ <- char '/'
  bottom <- int
  return $ (LispNumber . Rational) (top % bottom)

parseInteger :: Parser LispVal
parseInteger =  do
  int' <- many1 digit
  return $ (LispNumber . Integer . read) int'

parseNegInteger :: Parser LispVal
parseNegInteger =  do
  _ <-  char '-'
  int' <- many1 digit
  return $ (LispNumber . Integer . negate . read) int'

parseVector :: Parser LispVal
parseVector = do
  _ <- char '('
  vec <- sepBy parseExpr spaces1
  _ <- char ')'
  return $ Vector vec

parseList :: Parser LispVal
-- Parse lispExpr which hare seperated by one or more whitespace
parseList = List <$> sepBy parseExpr spaces1

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

parseNumber = try parseNegFloat
              <|> try parseNegRational
              <|> try parseNegInteger --TODO WENN MIR WAS UM DIE OHREN FLIEGT LIEGTS HIER DRAN
              <|> parseInteger -- etc

parseExpr :: Parser LispVal
parseExpr = try parseAtom --first try to parse a atom
         <|> try parseComplex
         <|> parseNumber
         <|> parseString -- if this fails try to parse a string
         <|> parseQuoted
         <|> do _ <- char '('
                -- parses a normal list until it encounter a dot, at which point it will go back and sstart to parse
                -- a dotted list
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x
