-- file: ch25/G.hs
mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    Pair n s       = foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s+x)-- file: ch25/G.hs
data Pair a b = Pair !a !b
