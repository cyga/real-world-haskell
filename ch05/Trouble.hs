-- file: ch05/Trouble.hs
upcaseFirst (c:cs) = toUpper c -- forgot ":cs" here

-- file: ch05/Trouble.hs
camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
