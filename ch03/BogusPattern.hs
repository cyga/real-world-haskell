-- file: ch03/BogusPattern.hs
data Fruit = Apple | Orange

apple = "apple"

orange = "orange"        

whichFruit :: String -> Fruit

whichFruit f = case f of
                 apple  -> Apple
                 orange -> Orange

-- file: ch03/BogusPattern.hs
equational apple = Apple
equational orange = Orange

-- file: ch03/BogusPattern.hs
betterFruit f = case f of
                  "apple"  -> Apple
                  "orange" -> Orange
