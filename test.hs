import Prelude hiding (lookup)
type Name = String
data Term = Var Name | Con Int | Add Term Term | Lam Name Term | App Term Term

data Value = Wrong | Num Int | Fun (Value -> IO Value) 
type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

eval :: Term -> Environment -> IO Value
eval (Var x) e = lookup x e
eval (Con i) e = pure (Num i)
--TODO REWRITE THIS TO DO NOTATION
--evaluate u; bind a to the result; evaluate v; bind b to the
--result; add a to b. The types work out: the calls to interp and add yield results of type
--M Value, and variables a and b have type Value.
eval (Add u v) e = eval u e >>= (\a -> eval v e >>= (\b ->add a b))
eval (Lam x v) e = pure (Fun (\a -> eval v ((x,a):e)))
eval (App t u) e = eval t e >>= (\f -> eval u e >>= (\a -> apply f a))


eval' :: Term -> Environment -> IO Value
eval' (Var x) e = lookup x e
eval' (Con i) e = pure (Num i)
eval' (Add u v) e = do
                a <- eval' u e 
                b <- eval' v e
                add a b
eval' (Lam x v) e = pure (Fun (\a -> eval v ((x,a):e)))
eval' (App t u) e = eval' t e >>= (\f -> eval' u e >>= (\a -> apply f a))

lookup :: Name -> Environment -> IO Value
lookup x [] = pure Wrong
lookup x ((y,b):e) = if x==y then pure b else lookup x e

add :: Value -> Value -> IO Value
add (Num i) (Num j) = pure (Num (i+j))
add a b = pure Wrong

apply :: Value -> Value -> IO Value
apply (Fun k) a = k a
apply f a = pure Wrong

test :: Term -> IO(String)
test t = do
    val <- (eval' t [])
    return $  showval val

