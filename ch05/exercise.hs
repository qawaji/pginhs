-- 5.1
powsum = sum [x^2| x <- [1..100]]

-- 5.2
grid :: Int -> Int -> [(Int, Int)]
grid nx ny = [(x, y) | x <- [0..nx], y <- [0..ny]]

-- 5.3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 5.4
myReplicate :: Int -> a -> [a]
myReplicate n a = [a| _ <- [1..n]]

-- 5.5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z)| x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x * 2]

-- 5.7

-- 5.8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x $ zip xs [0..]

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]