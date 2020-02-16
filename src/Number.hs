module Number where 
import Data.Complex --Complex
import Data.Ratio --Rational

data LispNumber = Integer Integer -- Stores a Haskell Integer
                | Real Double
                | Rational Rational
                | Complex (Complex Double) deriving(Eq)

instance Show LispNumber where show = showNumber

instance Num LispNumber where
  (Integer a)  + (Integer b) = (Integer $ a + b)
  (Real a)  + (Real b) = (Real $ a + b)
  (Rational a)  + (Rational b) = (Rational $ a + b)
  (Complex a)  + (Complex b) = (Complex $ a + b)

  (Integer a)  - (Integer b) = (Integer $ a - b)
  (Integer a)  - (Real b) = (Real $ fromInteger a - b)
  (Integer a)  - (Rational b) = (Rational $ fromInteger  a - b)
  (Integer a)  - (Complex b) = (Complex $ fromInteger a - b)

  (Real a)  - (Real b) = (Real $ a - b)
  (Rational a)  - (Rational b) = (Rational $ a - b)
  (Complex a)  - (Complex b) = (Complex $ a - b)

  (Integer a)  * (Integer b) = (Integer $ a * b)
  (Real a)  * (Real b) = (Real $ a * b)
  (Rational a)  * (Rational b) = (Rational $ a * b)
  (Complex a)  * (Complex b) = (Complex $ a * b)

  abs (Integer a)  = (Integer $ abs a )
  abs (Real a)     = (Real $ abs a )
  abs (Rational a) = (Rational $ abs a )
  abs (Complex a)   = (Complex $ abs a )

  fromInteger i = Integer i

  signum (Integer a)  = (Integer $ signum a )
  signum (Real a)     = (Real $ signum a )
  signum (Rational a) = (Rational $ signum a )
  signum (Complex a)   = (Complex $ signum a )

instance Fractional LispNumber where
  (Integer a) / (Integer b) = (Rational $ a % b)
  (Real a) / (Real b) = (Real $ a / b)
  (Complex a) / (Complex b) = (Complex $ a / b)
  (Rational a) / (Rational b) = (Rational $ a / b)

  fromRational i = Rational i

instance Ord LispNumber where
  compare (Integer a) (Integer b) = (compare a b)
  compare (Real a) (Real b) = (compare a b)
  compare (Rational a) (Rational b) = (compare a b)
  compare (Complex a) (Complex b) = error "Cant order complex numbers"


  
showNumber :: LispNumber -> String
showNumber ((Integer a))  = show a
showNumber ((Complex a))  = show a
showNumber ((Rational a)) = (show $ numerator a) ++ "/" ++ (show $ denominator a)
showNumber ((Real a))     = show a
