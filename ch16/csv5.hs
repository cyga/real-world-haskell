-- file: ch16/csv5.hs
eol = 
    do char '\n'
       char '\r' <|> return '\n'
