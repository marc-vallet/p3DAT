
{-# LANGUAGE CPP               #-}

module Web.IDL.Promise where
import qualified Control.Concurrent as Conc
import           Control.Exception
import qualified Web.FFI.Types as FFI

newtype Promise' = Promise' FFI.JSVal

data Promise a = Promise (FFI.JSVal -> IO a) Promise'

instance Functor Promise where
  fmap f (Promise fromJS p) = Promise (fmap f . fromJS) p

await' :: Promise' -> IO FFI.JSVal
await' p = do
  mv <- Conc.newEmptyMVar
  cb1 <- FFI.syncCallback1 FFI.ThrowWouldBlock $ \jval ->
    Conc.putMVar mv $ Right jval
  cb2 <- FFI.syncCallback1 FFI.ThrowWouldBlock $ \jmsg ->
    Conc.putMVar mv $ Left jmsg
  bind_promise p cb1 cb2
  val' <- Conc.takeMVar mv
  FFI.releaseCallback cb1
  FFI.releaseCallback cb2
  case val' of
      Right jval -> pure jval
      Left jmsg -> FFI.mkJSException jmsg >>= throwIO

await :: Promise a -> IO a
await (Promise fromJS p) =
  await' p >>= fromJS

#ifdef GHCJS_NEW
foreign import javascript safe
  "Web_Fetch.bind_promise"
  bind_promise :: Promise'
               -> FFI.Callback (FFI.JSVal -> IO ())
               -> FFI.Callback (FFI.JSVal -> IO ())
               -> IO ()
#else
bind_promise :: Promise'
             -> FFI.Callback (FFI.JSVal -> IO ())
             -> FFI.Callback (FFI.JSVal -> IO ())
             -> IO ()
bind_promise = undefined
#endif
