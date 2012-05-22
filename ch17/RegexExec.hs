-- file: ch17/RegexExec.hs
            if r < 0
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> ByteString -> ByteString
    substring x y _ | x == y = empty
    substring a b s = end
        where
            start = unsafeDrop (fromIntegral a) s
            end   = unsafeTake (fromIntegral (b-a)) start-- file: ch17/RegexExec.hs
    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off)
                         (fromIntegral len)
                         0
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)-- file: ch17/RegexExec.hs
match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr

    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)-- file: ch17/RegexExec.hs
match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]-- file: ch17/RegexExec.hs
capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
         c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
         return . fromIntegral =<< peek (n_ptr :: Ptr CInt)-- file: ch17/RegexExec.hs
foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt-- file: ch17/RegexExec.hs
foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt
