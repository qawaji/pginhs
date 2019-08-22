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