-- file: ch25/F.hs
{-# LANGUAGE BangPatterns #-}

-- file: ch25/F.hs
mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)       = foldl' k (0, 0) xs
    k (!n, !s) x = (n+1, s+x)
