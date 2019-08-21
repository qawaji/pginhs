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