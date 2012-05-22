-- file: ch04/Lambda.hs
unsafeHead = \(x:_) -> x-- file: ch04/Lambda.hs
safeHead (x:_) = Just x
safeHead _ = Nothing
