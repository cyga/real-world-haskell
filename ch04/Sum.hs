-- file: ch04/Sum.hs
nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0-- file: ch04/Sum.hs
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs-- file: ch04/Sum.hs
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs-- file: ch04/Sum.hs
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x-- file: ch04/Sum.hs
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc
