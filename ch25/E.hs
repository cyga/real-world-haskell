-- file: ch25/E.hs
import System.Environment
import Text.Printf
import Control.Parallel.Strategies

main = do
    [d] <- map read `fmap` getArgs
    printf "%f\n" (mean [1..d])

foldl'rnf :: NFData a => (a -> b -> a) -> a -> [b] -> a
foldl'rnf f z xs = lgo z xs
    where
        lgo z []     = z
        lgo z (x:xs) = lgo z' xs
            where
                z' = f z x `using` rnf

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl'rnf k (0, 0) xs
    k (n, s) x = (n+1, s+x) :: (Int, Double)
