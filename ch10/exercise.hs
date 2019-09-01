-- exercise 10.1
putStr' :: String -> IO ()
putStr' = sequence_ . map putChar

-- exercise 10.4
adder :: IO ()
adder = do putStr "How many number?"
           n <- getLine
           adderStep 0 (read n)

adderStep :: Int -> Int -> IO ()
adderStep t r = 
  if r == 0 then
    do putStr "The total is "
       putStrLn (show t)
       return ()
  else
    do n <- getLine
       adderStep (t + (read n)) (r - 1)

-- exercise 10.5
adder' :: IO ()
adder' = do putStr "How many number? "
            n <- getLine
            rs <- sequence (replicate (read n) getLine)
            putStr "The total is "
            putStrLn $ show $ sum $ map read rs