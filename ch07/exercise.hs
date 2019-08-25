-- 7.1
-- [f x | x <- xs, p x] を map, filterをつかって
myfunc f p xs = map f (filter p xs)

-- 7.2
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and $ map p xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or $ map p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = x:xs
                    | otherwise = dropWhile' p xs

-- 7.3
-- map をfoldrを使って
map' f = foldr (\x xs -> f x : xs) []

-- filter をfoldrを使って
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 7.5 
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y) 

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y