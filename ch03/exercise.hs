-- 練習問題2
bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5], [6, 7, 8]]

myAdd :: Int -> Int -> Int -> Int
myAdd x y z = x + y + z

myCopy :: a -> (a, a)
myCopy x = (x, x)

myApply :: (a -> b) -> a -> b
myApply f a = f a

-- 練習問題3
mySecond :: [a] -> a
mySecond xs = head (tail xs)

mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)

myPair :: a -> b -> (a, b)
myPair x y = (x, y)

myDouble :: Num a => a -> a
myDouble x = x * 2

myPalindrome :: Eq a => [a] -> Bool
myPalindrome xs = reverse xs == xs

myTwice :: (a -> a) -> a -> a
myTwice f x = f (f x)