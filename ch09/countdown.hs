import Data.List

-- 9.6 
-- 累乗演算子の追加
data Op = Add | Sub | Mul | Div | Pow

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
valid Pow _ _ = True

-- 代数的な性質を使って高速化したもの
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = x `mod` y == 0 && y /= 1 
valid' Pow x y = y /= 1

-- 9.5 
-- Subを常に有効に。zero divide の可能性が出てきたのでDivの条件を変更
valid'' :: Op -> Int -> Int -> Bool
valid'' Add _ _ = True
valid'' Sub _ _ = True
valid'' Mul _ _ = True
valid'' Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                      brak (Val n) = show n
                      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid' o x y]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- exercise 9.1
choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys ]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices' ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs ]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)| n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]
                  
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- 9.6
solutions'' :: [Int] -> Int -> (Expr, Int)
solutions'' ns n = head $ sortBy resultOrder [(e, abs (m - n)) | ns' <- choices ns, (e,m) <- results ns']
                      where 
                        resultOrder (e1, n1) (e2, n2) | n1 == n2 = EQ
                                                      | n1 < n2 = LT
                                                      | n1 > n2 = GT

-- 9.4 
-- 考えられるすべての式
exprsAll :: [Int] -> [Expr]
exprsAll = concat . map exprs . choices

-- 有効な式 validを代数的な特徴を使ったものにすると少なくなってしまうので注意
exprsValidAll :: [Int] -> [Expr]
exprsValidAll ns = [e | ns' <- choices ns, e <- exprs ns', eval e /= []]

main :: IO()
main = print (solutions'' [1,3,7,10,25,50] 831)
-- main = print (solutions' [1,3,7,10,25,50] 765)

-- 9.4 有効な式
-- main = print (length $ exprsValidAll [1,3,7,10,25,50])
