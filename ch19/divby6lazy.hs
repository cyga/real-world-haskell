-- file: ch19/divby6lazy.hs
divBy :: Integral a => a -> [a] -> [Either String a]
divBy numerator denoms = map (div' numerator) denoms
    where div' a 0 = Left "division by 0"
          div' a b = Right (div a b)
