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