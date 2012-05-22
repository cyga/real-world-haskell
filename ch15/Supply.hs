-- file: ch15/Supply.hs
runSupply :: Supply s a -> [s] -> (a, [s])

-- file: ch15/Supply.hs
next :: Supply s (Maybe s)

-- file: ch15/Supply.hs
module Supply
    (
      Supply
    , next
    , runSupply
    ) where

-- file: ch15/Supply.hs
import Control.Monad.State

newtype Supply s a = S (State [s] a)

-- file: ch15/Supply.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- file: ch15/Supply.hs
    deriving (Monad)

-- file: ch15/Supply.hs
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs

-- file: ch15/Supply.hs
showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)
