-- file: ch16/ParsecPlus.hs
instance MonadPlus (GenParser tok st) where
    mzero = fail "mzero"
    mplus = (<|>)
