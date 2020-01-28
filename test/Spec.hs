import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec hiding (spaces)
import Interpreter
import Data.Either

test1 = test [("parse \"test\"" ~: ((parse parseString "" "\"test\"")) ~?= (Right (String "test")))
             ]


main :: IO (Counts)
main = runTestTT test1
