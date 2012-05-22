-- file: ch03/NestedLets.hs
foo = let a = 1
      in let b = 2
         in a + b

-- file: ch03/NestedLets.hs
bar = let x = 1
      in ((let x = "foo" in x), x)

-- file: ch03/NestedLets.hs
quux a = let a = "foo"
         in a ++ "eek!"
