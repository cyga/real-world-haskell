-- file: ch17/SimpleFFI.hs
{-# LANGUAGE ForeignFunctionInterface #-}

-- file: ch17/SimpleFFI.hs
import Foreign
import Foreign.C.Types

-- file: ch17/SimpleFFI.hs
foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble

-- file: ch17/SimpleFFI.hs
fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))

-- file: ch17/SimpleFFI.hs
main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]
