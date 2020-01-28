{-# LANGUAGE FlexibleContexts #-}

import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec hiding (spaces, parseTest)
import Text.Parsec.Prim hiding(parseTest)--Stream
import Data.Functor.Identity --Identity
import Interpreter
import Data.Either


parseTest :: (Stream s Identity t) => Parsec s () LispVal -> s -> LispVal
parseTest p input = case parse p "" input of
                      Left err -> error $ show err 
                      Right x  -> x

-- Creates an equal test
eqT :: Parsec String () LispVal -> String -> LispVal  -> Test
eqT p s v = s ~: ((parseTest p s)) ~?= v

string = test [("parse \"test\"" ~: ((parse parseString "" "\"test\"")) ~?= (Right (String "test")))
             ]

number = test [("parse \"123\"" ~: ((parseTest parseNumber "123")) ~?= (Number 123)),
               eqT parseNumber "100" (Number 100),
               eqT parseNumber "1.22" (Number 100)
             ]

main :: IO (Counts)
main = runTestTT number
