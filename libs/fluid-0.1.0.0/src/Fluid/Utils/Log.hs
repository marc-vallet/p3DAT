
{-# LANGUAGE CPP                        #-}

module Fluid.Utils.Log
where
import           Web.Console

import           Control.Concurrent (myThreadId)
import qualified Control.Concurrent.STM as STM
import           Control.Monad (forM_, when)
import           Control.Monad.Writer.Strict
import qualified Data.DList as DL
import           Data.IORef
import qualified Data.Text as T

import           System.IO.Unsafe (unsafePerformIO)


class Monad m => TraceM m where
    logTagMsg' :: Maybe String -> String -> m ()

logMsg :: TraceM m => String -> m ()
logMsg = logTagMsg' Nothing

logTagMsg :: TraceM m => String -> String -> m ()
logTagMsg tag = logTagMsg' (Just tag)

---------------------------------------------------------------
-- Implementation and instances

type LogOptions = Bool

{-# NOINLINE logOptions #-}
logOptions :: IORef LogOptions
logOptions = unsafePerformIO $
    newIORef False

setLogOptions :: MonadIO m => LogOptions -> m ()
setLogOptions opts =
    liftIO $ writeIORef logOptions opts

getLogOptions :: MonadIO m => m LogOptions
getLogOptions =
    liftIO $ readIORef logOptions

instance TraceM IO where
    logTagMsg' mbtag msg = do
        opts <- readIORef logOptions
        when opts $ do
            tag <- maybe (show <$> myThreadId) pure mbtag
            consoleLog $ T.pack $ "---" <> tag <> ": " <> msg


newtype TraceSTM a =
    TraceSTM (WriterT (DL.DList (Maybe String, String)) STM.STM a)
  deriving (Functor, Applicative, Monad)

liftSTM :: STM.STM a -> TraceSTM a
liftSTM stm = TraceSTM $ lift stm

atomicallyTracing :: TraceSTM a -> IO a
atomicallyTracing (TraceSTM wm) = do
    (x, logs) <- STM.atomically $ runWriterT wm
    opts <- readIORef logOptions
    when opts $ do
        tid <- myThreadId
        let msg0 s = "---" ++ show tid ++ ": STM.atomically" ++ s
        consoleLog $ T.pack $ foldl tr msg0 logs ""
    pure x
    where
      tr acc (mbtag, msg) s =
        acc $ "\n---        " ++ (maybe "" id mbtag) ++ ": " ++ msg ++ s

instance TraceM TraceSTM where
    logTagMsg' mbtag msg =
      TraceSTM $
        tell $ DL.singleton (mbtag, msg)

