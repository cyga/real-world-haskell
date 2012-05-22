-- file: ch03/ListADT.hs
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)
