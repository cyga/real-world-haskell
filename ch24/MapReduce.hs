-- file: ch24/MapReduce.hs
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat-- file: ch24/MapReduce.hs
mapReduce
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c-- file: ch24/MapReduce.hs
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc-- file: ch24/MapReduce.hs
simpleMapReduce
    :: (a -> b)      -- map function
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
