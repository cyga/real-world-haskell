-- file: ch17/ForeignPtr.hs
unsafePerformIO :: IO a -> a-- file: ch17/ForeignPtr.hs
alloca :: Storable a => (Ptr a -> IO b) -> IO b-- file: ch17/ForeignPtr.hs
useAsCString :: ByteString -> (CString -> IO a) -> IO a-- file: ch17/ForeignPtr.hs
newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
