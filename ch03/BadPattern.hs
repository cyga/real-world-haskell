-- file: ch03/BadPattern.hs
badExample (x:xs) = x + badExample xs

-- file: ch03/BadPattern.hs
goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
