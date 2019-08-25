import Data.Char

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

-- 7.6
-- p 終了条件
-- h データの加工
-- t 変数の変更
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int

testBits :: [Bit]
testBits = [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f = unfold null (f . head) tail

unfoldIterate f = unfold (\_ -> False) id f

-- 7.9 altMap
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- 7.10
luhn :: [Int] -> Bool
luhn xs = total `mod` 10 == 0
  where
    total = sum $ altMap luhnSingle luhnDouble $ reverse xs    
    luhnSingle n = n `mod` 9
    luhnDouble n = (n * 2) `mod` 9
