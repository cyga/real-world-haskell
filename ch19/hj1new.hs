-- file: ch19/hj1.hs
{-# LANGUAGE ScopedTypeVariables #-}
import Control.OldException

handlerArith :: ArithException -> IO ()
handlerArith _ = putStrLn "Caught arith exception"

handlerSome :: SomeException -> IO ()
handlerSome _ = putStrLn "Caught some exception"

safePrint :: Integer -> IO ()
safePrint x = (print x) `catches` [
    Handler (\ (ex :: ArithException ) -> handlerArith ex)
    , Handler (\ (ex :: SomeException ) -> handlerSome ex)]
