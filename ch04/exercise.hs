-- 練習問題1
-- プレリュード関数を使って長さが偶数のリストを半分ずつに分割する関数を書く
halve :: [a] -> ([a], [a])
halve xs = (take hlen xs, drop hlen xs)
  where hlen = (length xs) `div` 2

-- 練習問題2
-- third :: [a] -> a の3つの実装
-- head と tail を使って
thirdA :: [a] -> a
thirdA = head . tail . tail 

-- !! をつかって
thirdB :: [a] -> a
thirdB xs = xs !! 2

-- パターンマッチ
thirdC :: [a] -> a
thirdC (_:_:x:xs) = x

-- 練習問題3
-- 空リストのときは空リストを返す安全なsafetailの実装
-- safetail :: [a] -> [a]
-- tailは先頭の要素を覗いたリストを返す
-- 条件式を使って
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

-- ガードを使って
safetailB :: [a] -> [a]
safetailB xs | null xs = []
             | otherwise = tail xs

-- パターンマッチ
safetailC :: [a] -> [a]
safetailC [] = []
safetailC (x:xs) = xs

-- 練習問題4
-- パターンマッチをつかって||を4通り実装
orA :: Bool -> Bool -> Bool
False `orA` False = False
False `orA` True = True
True `orA` False = True
True `orA` True = True

orB :: Bool -> Bool -> Bool
False `orB` False = False
_ `orB` _ = True

orC :: Bool -> Bool -> Bool
True `orC` _ = True
False `orC` b = b

orD :: Bool -> Bool -> Bool
b `orD` c | b == c = b
          | otherwise = True