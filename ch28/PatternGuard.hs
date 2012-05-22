-- file: ch28/PatternGuard.hs
{-# LANGUAGE PatternGuards #-}

testme x xs | Just y <- lookup x xs, y > 3 = y
            | otherwise                    = 0

-- file: ch28/PatternGuard.hs
testme_noguards x xs = case lookup x xs of
                         Just y | y > 3 -> y
                         _              -> 0
