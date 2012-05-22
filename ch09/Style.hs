-- file: ch09/Style.hs
goodWhere = take 5 lambdas
    where lambdas = []

alsoGood =
    take 5 lambdas
  where
    lambdas = []

badWhere =           -- legal, but ugly and hard to read
    take 5 lambdas
    where
    lambdas = []-- file: ch09/Style.hs
normalIndent =
    undefined

strangeIndent =
                           undefined-- file: ch09/Style.hs
unusualPunctuation =
    [ (x,y) | x <- [1..a], y <- [1..b] ] where {
                                           b = 7;
 a = 6 }

preferredLayout = [ (x,y) | x <- [1..a], y <- [1..b] ]
    where b = 7
          a = 6-- file: ch09/Style.hs
commonDo = do
  something <- undefined
  return ()

-- not seen very often
rareDo =
  do something <- undefined
     return ()-- file: ch09/Style.hs
weirdLet = let foo = undefined
               bar = foo * 2
    in undefined

strangeLet = let foo = undefined
                 bar = foo * 2 in
    undefined-- file: ch09/Style.hs
tidyLet = let foo = undefined
              bar = foo * 2
          in undefined
