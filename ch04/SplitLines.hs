-- file: ch04/SplitLines.hs
fixLines :: String -> String
fixLines input = unlines (splitLines input)-- file: ch04/SplitLines.hs
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'-- file: ch04/SplitLines.hs
splitLines :: String -> [String]
