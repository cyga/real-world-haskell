-- file: ch11/Arbitrary.hs
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (x, y)-- file: ch11/Arbitrary.hs
instance Arbitrary Ternary where
  arbitrary     = elements [Yes, No, Unknown]-- file: ch11/Arbitrary.hs
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)-- file: ch11/Arbitrary.hs
  elements :: [a] -> Gen a
  choose   :: Random a => (a, a) -> Gen a
  oneof    :: [Gen a] -> Gen a-- file: ch11/Arbitrary.hs
class Arbitrary a where
  arbitrary   :: Gen a
