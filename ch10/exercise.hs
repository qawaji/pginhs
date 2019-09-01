import System.IO

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

-- exercise 10.6
getCh :: IO Char              
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLine' ""

readLine' :: String -> IO String
readLine' buf = 
  do x <- getCh
     case x of '\n' -> do putChar '\n'
                          return buf
               '\DEL' -> if null buf then
                            readLine' ""
                         else
                            do putChar '\b'                  
                               readLine' (init buf)
               _ -> do putChar x
                       readLine' (buf ++ [x])
