-- 練習問題3 
-- productではPreludeのものと名前がかぶるのでmyProductで実装
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- 練習問題4
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x]

-- 練習問題5
-- 等号がなくなると重複している要素がなくなる
qsort2 [] = []
qsort2 (x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger = [b | b <- xs, b > x]