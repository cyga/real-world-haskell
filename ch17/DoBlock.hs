-- file: ch17/DoBlock.hs
alloca $ \stringptr -> do
   ... call some Ptr CString function
   peek stringptr-- file: ch17/DoBlock.hs
useAsCString str $ \cstr -> do
   ... operate on the C string
   ... return a result
