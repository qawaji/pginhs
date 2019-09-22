-- 15.4
fibs :: [Integer]
fibs = 0:1:[x1 + x2 | (x1,x2) <- zip fibs (tail fibs)]

