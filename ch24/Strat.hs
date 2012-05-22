-- file: ch24/Strat.hs
vectorSum' :: (NFData a, Num a) => [a] -> [a] -> [a]
vectorSum' = parZipWith rnf (+)-- file: ch24/Strat.hs
using :: a -> Strategy a -> a
using x s = s x `seq` x-- file: ch24/Strat.hs
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat-- file: ch24/Strat.hs
parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` (parList strat xs)-- file: ch24/Strat.hs
instance NFData Char
instance NFData Int

instance NFData a => NFData (Maybe a) where
    rnf Nothing  = ()
    rnf (Just x) = rnf x

{- ... and so on ... -}-- file: ch24/Strat.hs
class NFData a where
  rnf :: Strategy a
  rnf = rwhnf-- file: ch24/Strat.hs
rwhnf :: Strategy a 
rwhnf x = x `seq` ()-- file: ch24/Strat.hs
r0 :: Strategy a 
r0 _ = ()-- file: ch24/Strat.hs
type Done = ()

type Strategy a = a -> Done
