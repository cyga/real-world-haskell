-- file: ch06/NewtypeDiff.hs
-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: type parameters are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
    }

-- bad: no fields
newtype TooFew = TooFew

-- bad: more than one field
newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
newtype TooManyCtors = Bad Int
                     | Worse Int
