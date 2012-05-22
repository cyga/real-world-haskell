-- file: ch28/STMPlus.hs
instance MonadPlus STM where
  mzero = retry
  mplus = orElse

-- file: ch28/STMPlus.hs
msum :: MonadPlus m => [m a] -> m a
msum =  foldr mplus mzero
