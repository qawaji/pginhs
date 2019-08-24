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