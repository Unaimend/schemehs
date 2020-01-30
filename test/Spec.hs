{-# LANGUAGE FlexibleContexts #-}

import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec hiding (spaces, parseTest)
import Text.Parsec.Prim hiding(parseTest)--Stream
import Data.Functor.Identity --Identity
import Interpreter
import Data.Either
import LispData
import FileIO

interp x = primitiveBindings >>= flip evalString x

parseTest :: (Stream s Identity t) => Parsec s () LispVal -> s -> LispVal
parseTest p input = case parse p "" input of
                      Left err -> error $ show err
                      Right x  -> x

-- Creates an equal test
eqT :: Parsec String () LispVal -> String -> LispVal  -> Test
eqT p s v = s ~: ((parseTest p s)) ~?= v

string = test [("parse \"test\"" ~: ((parse parseString "" "\"test\"")) ~?= (Right (String "test")))
             ]

numberParse = test [("parse \"123\"" ~: ((parseTest parseNumber "123")) ~?= (Number 123)),
               eqT parseNumber "100" (Number 100),
               eqT parseNumber "(-100)" (Number  (-100))
             ]

testInter ::  String -> String -> IO Test
testInter x s = ( ~=? x) <$> (interp s)

numberInterp = [ testInter "3" "(+ 0 3)",
                 testInter "3" "(+ 3 0)",
                 testInter "6" "(+ 3 3)",
                 testInter "0" "(- 3 3)",
                 testInter "#t" "(number? 3)",
                 testInter "#t" "(number? 0)",
                 testInter "#f" "(number? \"3\")",
                 testInter "#f" "(number? \'())",
                 testInter "#f" "(number? \"TestString\")"
               ]

main :: IO (Counts)
main =do runTestTT numberParse
         test <$> sequenceA numberInterp >>= runTestTT
