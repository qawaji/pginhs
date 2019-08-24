-- 6.1
-- 負の値の場合はそのまま返す
fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac' (n - 1)
       | n < 0 = n

-- 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 6.3
-- ^ や ^^ だとambiguousになっちゃうので ^^^ で定義
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n - 1))

-- 6.4
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a > b = euclid b (a - b)
           | a < b = euclid a (b - a)

-- 6.5

-- 6.6
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys