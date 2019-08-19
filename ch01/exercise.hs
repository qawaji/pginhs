-- 練習問題３ 
-- productではPreludeのものと名前がかぶるのでmyProductで実装
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs