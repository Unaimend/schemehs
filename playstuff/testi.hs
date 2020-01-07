import Prelude hiding(seq)
type Parser a = String -> [(a,String)]

result :: a -> Parser a
result v = \inp -> [(v,inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)]

seq :: Parser a -> Parser b -> Parser (a,b)
seq p q = \inp -> [((v,w),inp'') | (v,inp') <- p inp
                                 , (w,inp'') <- q inp']

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp]

sat :: (Char -> Bool) -> Parser Char
-- Warum ist das erste item da
sat p = item `bind` \x ->
        if p x then result x else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> ’0’ <= x && x <= ’9’)

lower :: Parser Char
lower = sat (\x -> ’a’ <= x && x <= ’z’)

upper :: Parser Char
upper = sat (\x -> ’A’ <= x && x <= ’Z’)

main :: IO ()
main = do
  -- bind verstehen, aka list comprehension in Haskell
  let parser =
        item `bind` \x1 ->
        item `bind` \x2 ->
        result x2

  print $ parser "test"
  return ()
