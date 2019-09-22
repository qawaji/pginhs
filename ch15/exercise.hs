-- 15.4
fibs :: [Integer]
fibs = 0:1:[x1 + x2 | (x1,x2) <- zip fibs (tail fibs)]

-- 15.5
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeat' :: a -> Tree a
repeat' x = xs where xs = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _            = Leaf
take' _ Leaf         = Leaf
take' n (Node l x r) = Node (take' (n - 1) l) x (take' (n - 1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

-- 15.6
sqroot :: Double -> Double
sqroot n = v
  where
    next a = (a + n/a) / 2
    iter = iterate next 1
    (v, _) = head $ dropWhile (\(x, y) -> abs (x - y) > 0.00001 )[(x, y) | (x, y) <- zip iter (tail iter)]