-- exercise 9.2
removeone :: Eq a => [a] -> a -> [a]
removeone [] _ = []
removeone (x:xs) y | x == y = xs
                   | otherwise = x : removeone xs y

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = x `elem` ys && isChoice xs ys