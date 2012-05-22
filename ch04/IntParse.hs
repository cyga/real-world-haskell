-- file: ch04/IntParse.hs
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs-- file: ch04/IntParse.hs
loop acc [] = acc-- file: ch04/IntParse.hs
loop :: Int -> String -> Int

asInt xs = loop 0 xs-- file: ch04/IntParse.hs
import Data.Char (digitToInt) -- we'll need ord shortly

asInt :: String -> Int
