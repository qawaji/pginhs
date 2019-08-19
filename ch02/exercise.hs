-- 練習問題3
-- N -> n 小文字に
-- xs を一時下げる　オフサイドルール
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]


-- 練習問題4
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myLast2 xs = xs !! (length xs - 1) 