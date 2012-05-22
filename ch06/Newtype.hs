-- file: ch06/Newtype.hs
data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)

-- file: ch06/Newtype.hs
newtype UniqueID = UniqueID Int
    deriving (Eq)
