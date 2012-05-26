-- file: ch24/ModifyMVar.hs
import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.OldException (block, catch, throw, unblock)
import Prelude hiding (catch) -- use Control.OldException's version

modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io = 
  block $ do
    a <- takeMVar m
    (b,r) <- unblock (io a) `catch` \e ->
             putMVar m a >> throw e
    putMVar m b
    return r
