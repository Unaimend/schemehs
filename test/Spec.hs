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

integerParse = test [("parse \"123\"" ~: ((parseTest parseInteger "123")) ~?= (LispNumber (Integer 123))),
               eqT parseInteger "100" (LispNumber $ Integer $ 100),
               eqT parseInteger "(- 100)" (LispNumber $ Integer $  (-100)),
               --eqT parseInteger "(-100)" (Integer  (-100)) this one should fail 
               eqT parseInteger "-100" (LispNumber $ Integer $ (-100))
             ]

testInter ::  String -> String -> IO Test
testInter x s = (s ~:) <$> (( ~=? x) <$> (interp s))

integerInterp = [testInter "3" "(+ 0 3)",
                 testInter "3" "(+ 3 0)",
                 testInter "6" "(+ 3 3)",
                 testInter "0" "(- 3 3)",
                 testInter "0" "(+ 3 (- 3))",
                 testInter "-3" "(+ 0 (- 3))",
                 testInter "3" "(+ 3 (- 0))",
                 --Typetesting functions number
                 testInter "#t" "(number? 3 3 3)",
                 testInter "#t" "(number? 3)",
                 testInter "#t" "(number? 0)",
                 testInter "#t" "(number? (- 3))",
                 --Typetesting functions integer
                 testInter "#t" "(integer? 3 3 3)",
                 testInter "#t" "(integer? 3)",
                 testInter "#t" "(integer? (- 3))",
                 testInter "#t" "(integer? 0)",
                 testInter "#f" "(integer? \"3\")",
                 testInter "#f" "(integer? \'())",
                 testInter "#f" "(integer? \"TestString\")",
                 testInter "#f" "(integer? (define (x) (+ x x)))",
                 --eventesting
                 testInter "#t" "(even? 2)",
                 testInter "#t" "(even? 0)",
                 testInter "#t" "(even? (- 2))",
                 testInter "#f" "(even? 3)",
                 testInter "#t" "(even? (- 0))",
                 testInter "#f" "(even? (- 3))",
                 testInter "#t" "(even? \"2\")",
                 --eventesting
                 testInter "#f" "(odd? 2)",
                 testInter "#f" "(odd? 0)",
                 testInter "#f" "(odd? (- 2))",
                 testInter "#t" "(odd? 3)",
                 testInter "#f" "(odd? (- 0))",
                 testInter "#t" "(odd? (- 3))",
                 testInter "#f" "(odd? \"2\")",
                 --Typetesting functions rational
                 testInter "#t" "(rational? 3 3 3)",
                 testInter "#t" "(rational? 3)",
                 testInter "#t" "(rational? 0)"  ,
                 testInter "#t" "(rational? (- 3))",
                 --Typetesting functions real
                 testInter "#t" "(real? 3 3 3)",
                 testInter "#t" "(real? 3)",
                 testInter "#t" "(real? 0)",
                 testInter "#t" "(real? (- 3))",
                 --Typetesting functions complex
                 testInter "#t" "(complex? 3 3)",
                 testInter "#t" "(complex? 3 3 3)",
                 testInter "#t" "(complex? 3)",
                 testInter "#t" "(complex? 0)",
                 testInter "#t" "(complex? (- 3))"
               ]

--stdLibTest = []
main :: IO (Counts)
main =do runTestTT integerParse
         test <$> sequenceA integerInterp >>= runTestTT
