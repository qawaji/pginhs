-- 練習問題3
-- N -> n 小文字に
-- xs を一時下げる　オフサイドルール
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]


-- 練習問題4
myLast xs = xs !! (length xs - 1) 

myLastR (x:[]) = x
myLastR (x:xs) = myLastR xs



-- 練習問題5
myInit xs = take (length xs - 1) xs

myInitR (x:[]) = []
myInitR (x:xs) = x : myInitR xs