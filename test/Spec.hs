{-# LANGUAGE FlexibleContexts #-}

import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec hiding (spaces, parseTest)
import Text.Parsec.Prim hiding(parseTest)--Stream
import Data.Functor.Identity --Identity
import Interpreter
import Data.Either
import Data.Ratio
import LispData
import FileIO
import Number

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
               --eqT parseExpr "(- 100)" (LispNumber $ Integer $  (-100)), TODO FIX LATER
               --eqT parseInteger "(-100)" (Integer  (-100)) this one should fail 
               eqT parseNegInteger "-100" (LispNumber $ Integer $ (-100)),
               eqT parseNegInteger "-100" (LispNumber $ Integer $ (-100)),
               eqT parseNegRational "-100/1" (LispNumber $ Rational $ (-100 % 1)),
               eqT parseNegRational "-25/1" (LispNumber $ Rational $ (-50 % 2)),
               eqT parseNegRational "100/1" (LispNumber $ Rational $ (100 % 1)),
               eqT parseNegRational "50/2" (LispNumber $ Rational $ (25 % 1)),
               eqT parseNegFloat "5.2" (LispNumber $ Real $ 5.2),
               eqT parseNegFloat "-5.2" (LispNumber $ Real $ (- 5.2))
             ]

testInter ::  String -> String -> IO Test
testInter x s = (s ~:) <$> (( ~=? x) <$> (interp s))

integerInterp = [-- math function integer
                 testInter "3" "(+ 0 3)",
                 testInter "3" "(+ 3 0)",
                 testInter "6" "(+ 3 3)",
                 testInter "9" "(+ 3 3 3)",
                 testInter "6" "(+ 1 2 3)",
                 testInter "6" "(* 1 2 3)",
                 testInter "0" "(* 0 1)",
                 testInter "0" "(* 1 0)",
                 testInter "5" "(* 1 5)",
                 testInter "5" "(* 5 1)",
                 testInter "0" "(- 3 3)",
                 testInter "0" "(+ 3 (- 3))",
                 testInter "-3" "(+ 0 (- 3))",
                 testInter "3" "(+ 3 (- 0))",
                 testInter "3" "(- -3)",

                 testInter  "4" "(floor 4)",
                 testInter  "4" "(ceil 4)",
                 -- math function rational
                 testInter "6/1" "(+ 3/1 3/1)",
                 testInter "0/1" "(+ 0/1 0/1)",
                 testInter "1/1" "(* 4/2 1/2)",
                 testInter "0/1" "(* 0/2 1/2)",
                 testInter "1/2" "(* 1/1 1/2)",
                 testInter "3/2" "(- -3/2)",
                 testInter "4" "(floor 9/2)" ,
                 testInter "5" "(ceil 9/2)" ,
                 -- math function real 
                 testInter "6.0" "(+ 3.0 3.0)",
                 testInter "0.0" "(+ 0.0 0.0)",
                 testInter "1.0" "(* 2.0 0.5)",
                 testInter "0.0" "(* 0.0 0.5)",
                 testInter "0.5" "(* 1.0 0.5)",
                 testInter "0.3" "(- -0.3)",
                 testInter  "4" "(floor 4.5)",
                 testInter  "5" "(ceil 4.5)",
                 --math function trg
                 testInter "0.0" "(sin 0.0)",
                 testInter "1.0" "(cos 0.0)",
                 testInter "0.0" "(tan 0.0)",
                 testInter "0.0" "(log 1.0)",
                 --math rounding functions
                 testInter "2" "(floor 2.5)",
                 testInter "3" "(ceil 2.5)",
                 testInter "3" "(round 2.6)",
                 testInter "2" "(round 2.4)",
                 testInter "2" "(truncate 2.4)",
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
                 --even testing
                 testInter "#t" "(even? 2)",
                 testInter "#t" "(even? 0)",
                 testInter "#t" "(even? (- 2))",
                 testInter "#f" "(even? 3)",
                 testInter "#t" "(even? (- 0))",
                 testInter "#f" "(even? (- 3))",
                 testInter "#t" "(even? \"2\")",
                 --odd testing
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

testEqv = [testInter "#t" "(eqv? #t #t)",
           testInter "#t" "(eqv? #f #f)",
           testInter "#t" "(eqv? (eqv? 0 0) (= 0 0))",
           testInter "#t" "(eqv? (eqv? 1 1) (= 1 1))",
           testInter "#t" "(eqv? (eqv? -1 -1) (= -1 -1))",
           testInter "#t" "(eqv? (eqv? 0 1) (= 0 1))",
           testInter "#t" "(eqv? (eqv? 1 -1) (= 1 -1))",
           testInter "#t" "(eqv? (eqv? -1 1) (= -1 1))",
           testInter "#t" "(eqv? '() '())",
           testInter "#f" "(eqv? #f #t)",
           testInter "#f" "(eqv? #t #f)"
          ]

testExact = [testInter "#t" "(exact? 4)",
             testInter "#t" "(exact? 0)",
             testInter "#t" "(exact? -4)",
             testInter "#f" "(exact? #f)"
            ]
--stdLibTest = []
main :: IO (Counts)
main =do runTestTT integerParse
         test <$> sequenceA integerInterp >>= runTestTT
         test <$> sequenceA testEqv >>= runTestTT
         test <$> sequenceA testExact >>= runTestTT
