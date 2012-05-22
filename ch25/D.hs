-- file: ch25/D.hs
mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl' k (0, 0) xs
    k (n, s) x = n `seq` s `seq` (n+1, s+x)
