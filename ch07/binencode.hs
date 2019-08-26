import Data.Char

-- 7.8, 7.9

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0
--bin2int bits = sum [w*b | (w, b) <- zip weights bits]
--  where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

chop8 :: [Bit] -> [[Bit]]
chop8 = chop 8

chop9 :: [Bit] -> [[Bit]]
chop9 = chop 9

decode :: [Bit] -> String
decode = map (chr . bin2int . getBits) . chop9
  where
    getBits bs | parityCheck bs = tail bs
               | otherwise = error "error parity check"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

err_channel :: [Bit] -> [Bit]
err_channel = tail

countBit :: [Bit] -> Int
countBit = length . filter (\x -> x == 1)

parityBit :: [Bit] -> Bit
parityBit bs | odd $ countBit bs = 1
             | otherwise = 0

addParityBit :: [Bit] -> [Bit]
addParityBit bs = (parityBit bs):bs

parityCheck :: [Bit] -> Bool
parityCheck bs = (parityBit $ tail bs) == head bs