-- file: ch04/SuffixTree.hs
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

-- file: ch04/SuffixTree.hs
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []

-- file: ch04/SuffixTree.hs
suffixes2 xs = init (tails xs)

-- file: ch04/SuffixTree.hs
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- file: ch04/SuffixTree.hs
suffixes3 xs = compose init tails xs

-- file: ch04/SuffixTree.hs
suffixes4 = compose init tails

-- file: ch04/SuffixTree.hs
suffixes5 = init . tails
