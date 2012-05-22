-- file: ch13/Monoid.hs
class Monoid a where
    mempty  :: a                -- the identity
    mappend :: a -> a -> a      -- associative binary operator

-- file: ch13/Monoid.hs
instance Monoid [a] where
    mempty  = []
    mappend = (++)

-- file: ch13/Monoid.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
                   
newtype AInt = A { unA :: Int }
    deriving (Show, Eq, Num)

-- monoid under addition
instance Monoid AInt where
    mempty = 0
    mappend = (+)

newtype MInt = M { unM :: Int }
    deriving (Show, Eq, Num)

-- monoid under multiplication
instance Monoid MInt where
    mempty = 1
    mappend = (*)
