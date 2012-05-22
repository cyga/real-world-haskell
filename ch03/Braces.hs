-- file: ch03/Braces.hs
bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1;  b = 2;
        c = 3 }
      in a + b + c
