-- file: ch03/Indentation.hs
foo = let firstDefinition = blah blah
          -- a comment-only line is treated as empty
                              continuation blah

          -- we reduce the indentation, so this is a new definition
          secondDefinition = yada yada

                             continuation yada
      in whatever
