-- 抽象機械
-- exercise 8.9 乗算の追加
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : ADD 0 : c)
eval (Mul x y) c = eval x (EVAL y : MUL 1 : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : ADD _ : c) n = eval y (ADD n : c)
exec (EVAL y : MUL _ : c) n = eval y (MUL n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MUL n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []

-- exercise 8.5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- exercise 8.6
eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)