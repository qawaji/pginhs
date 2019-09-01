-- exercise 10.1
putStr' :: String -> IO ()
putStr' = sequence_ . map putChar