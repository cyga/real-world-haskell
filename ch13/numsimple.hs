-- file: ch13/numsimple.hs
-- The "operators" that we're going to support
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

{- The core symbolic manipulation type -}
data SymbolicManip a = 
          Number a           -- Simple number, such as 5
        | Arith Op (SymbolicManip a) (SymbolicManip a)
          deriving (Eq, Show)

{- SymbolicManip will be an instance of Num.  Define how the Num
operations are handled over a SymbolicManip.  This will implement things
like (+) for SymbolicManip. -}
instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b
    a * b = Arith Mul a b
    negate a = Arith Mul (Number (-1)) a
    abs a = error "abs is unimplemented"
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)
