
module Fluid.Utils.Conc
    ( ThreadId, myThreadId, forkJSM, forkJSMs, killThread
    , threadDelay
    , IORef, newIORef, readIORef
    , writeIORef, atomicWriteIORef
    , atomicModifyIORef, atomicModifyIORef', atomicModifyIORef'_
    , MVar, newEmptyMVar, newMVar, takeMVar, putMVar, readMVar
    , module Control.Monad.STM
    , module Control.Concurrent.STM.TVar
    )
where
import           Fluid.Utils.Log (getLogOptions, logMsg)

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.IORef


------------------------------------
-- Extended concurrency to IO

-- | Run given `IO` action asynchronously, in a separate thread.
forkJSM :: IO () -> IO ThreadId
forkJSM a =
  head <$> forkJSMs [a]

forkJSMs :: [IO ()] -> IO [ThreadId]
forkJSMs [] = pure []
forkJSMs ios = do
    lopt <- getLogOptions
    if lopt then do
        sync <- newEmptyMVar
        tids <- flip mapM ios $ \io -> forkIO $ do
                        () <- readMVar sync
                        io
                        logMsg "end"
        logMsg $ "start " <> show tids
        putMVar sync ()
        pure tids
    else
        mapM forkIO ios


------------------------------------

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref (flip (,) () . f)

