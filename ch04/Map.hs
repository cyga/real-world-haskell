-- file: ch04/Map.hs
myMap :: (a -> b) -> [a] -> [b]

myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []-- file: ch04/Map.hs
square2 xs = map squareOne xs
    where squareOne x = x * x

upperCase2 xs = map toUpper xs-- file: ch04/Map.hs
import Data.Char (toUpper)

upperCase :: String -> String

upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []-- file: ch04/Map.hs
square :: [Double] -> [Double]

square (x:xs) = x*x : square xs
square []     = []
