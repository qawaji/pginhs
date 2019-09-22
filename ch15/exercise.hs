-- 15.4
fibs :: [Integer]
fibs = 0:1:[x1 + x2 | (x1,x2) <- zip fibs (tail fibs)]

-- 15.6
sqroot :: Double -> Double
sqroot n = v
  where
    next a = (a + n/a) / 2
    iter = iterate next 1
    (v, _) = head $ dropWhile (\(x, y) -> abs (x - y) > 0.00001 )[(x, y) | (x, y) <- zip iter (tail iter)]