-- file: ch06/AutomaticDerivation.hs
data CannotShow = CannotShow
                deriving (Show)

-- will not compile, since CannotShow is not an instance of Show
data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)
