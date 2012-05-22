-- file: ch03/Nullable.hs
wrapped = Just (Just "wrapped")-- file: ch03/Nullable.hs
someBool = Just True

someString = Just "something"-- file: ch03/Nullable.hs
data Maybe a = Just a
             | Nothing
