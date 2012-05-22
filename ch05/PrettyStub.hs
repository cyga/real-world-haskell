-- file: ch05/PrettyStub.hs
import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

-- file: ch05/PrettyStub.hs
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

-- file: ch05/PrettyStub.hs
hcat :: [Doc] -> Doc
hcat xs = undefined

-- file: ch05/PrettyStub.hs
fsep :: [Doc] -> Doc
fsep xs = undefined
