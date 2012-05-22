-- file: ch13/Append.hs
(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : xs ++ ys
_      ++ ys = ys
