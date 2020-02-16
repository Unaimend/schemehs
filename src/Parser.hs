module Parser where

import Data.Ratio
import Control.Monad

import Number
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Number 
import Text.Parsec.Error
import Debug.Trace
import LispData
--TODO Learn monads again and check what mapM is doing

-- Recognizes if a character is a valid scheme symbol
symbol :: Parser Char
symbol = oneOf "-.!#$%&|*+/:<=>?@^_~"

--symb = [".","!","#","$","%","&","|","*","+","/",":","<","=",">","?","@","^","_","~", "- ", "#"]
--lett = (map (:[]) $ ['a' .. 'z']) ++ (map (:[]) ['A' .. 'Z'])
--symlpars = map string (symb ++ lett) -- ++ (map show [ 0 .. 10])

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
  first <- trace "FUUC" ((letter <|> symbol) <?> "I HATE PARSEC")
  --first <- choice symlpars
  --the following chars must be one of letter, digit or symbol
  rest <-  (many (letter <|> digit <|> symbol)) 
  let atom = first :rest
  --catch special atoms
  case atom of "#t" -> return $ Bool True
               "#f" -> return $ Bool False
               ('-':x:_)   -> case x of
                   ' ' -> return $ Atom atom
                   _   -> do x <- parseNumber
                             return x--return $ (LispNumber . Integer . read) atom --TODO THIS IS NOT GOOD, try to parse -3o
               _    -> {-trace ("attom"++ show atom)-} (return $ Atom atom)

parseNegFloat :: Parser LispVal
parseNegFloat = do
  s <- trace "Float" sign
  beforeDot <- int
  dot <- char '.' <?> "Floating Point Parse Error: expecting ."
  afterDot <- int
  let d = s (read ((show beforeDot) ++ "." ++ (show afterDot)))
  return $ (LispNumber . Real) d

parseNegRational :: Parser LispVal
parseNegRational = do
  top <- trace "Ration" int
  _ <- char '/'
  bottom <- int
  return $ (LispNumber . Rational) (top % bottom)

parseInteger :: Parser LispVal
parseInteger =  do
  int' <- trace "Int " (many1 digit)
  return $ (LispNumber . Integer . read) int'

parseNegInteger :: Parser LispVal
parseNegInteger =  do
  _ <-  trace "NegInt" (char '-')
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

parseNumber = (try parseNegFloat <?> "DdwdS")
         <|> try parseNegRational
         <|> try parseNegInteger --TODO WENN MIR WAS UM DIE OHREN FLIEGT LIEGTS HIER DRAN
         <|> parseInteger -- etc

parseExpr :: Parser LispVal
parseExpr = try parseAtom --first try to parse a atom
         <|> parseNumber
         <|> parseString -- if this fails try to parse a string
         <|> parseQuoted
         <|> do _ <- char '('
                -- parses a normal list until it encounter a dot, at which point it will go back and sstart to parse
                -- a dotted list
                x <- trace "LIST?>" (try parseList <|> parseDottedList)
                _ <- char ')'
                return x


