-- file: ch08/SumFile.hs
main = do
    contents <- getContents
    print (sumFile contents)
  where sumFile = sum . map read . words
