
{-# LANGUAGE CPP        #-}

module Web.HTML.Window
    ( module Web.HTML.Window
    , module Web.Event.EventTarget
    )
where
import           Web.HTML.HTMLDocument
import           Web.Event.EventTarget
import           Web.FFI.Types

import           Data.IORef


newtype Window = Window JSVal

instance IsEventTarget Window

fromEventTarget :: IsEventTarget o => o -> Maybe Window
fromEventTarget obj =
  let EventTarget jobj = toEventTarget obj
  in Window <$> unsafeReadProtoTagged "Window" jobj

---------------------------------------------------

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(() => { return globalThis.window; })"
  window :: IO Window

foreign import javascript unsafe
  "((wnd) => { return wnd.document; })"
  document :: Window -> IO HTMLDocument

foreign import javascript unsafe
  "((wnd) => { return wnd.innerWidth; })"
  innerWidth :: Window -> IO Int

foreign import javascript unsafe
  "((wnd) => { return wnd.innerHeight; })"
  innerHeight :: Window -> IO Int

#else
window :: IO Window
window = undefined

document :: Window -> IO HTMLDocument
document = undefined

innerWidth :: Window -> IO Int
innerWidth = undefined

innerHeight :: Window -> IO Int
innerHeight = undefined
#endif


requestAnimationFrame :: (Double -> IO Bool) -> Window -> IO (IO ())
requestAnimationFrame fun win = do
  contr <- newIORef True
  cbr <- newIORef undefined
  let fun' jtimestamp = do
        let timestamp = fromJSDouble jtimestamp
        cont1 <- readIORef contr
        cont2 <- if cont1 then fun timestamp
                          else pure False
        cb <- readIORef cbr
        if cont2 then do
          _ <- js_requestAnimationFrame cb win
          pure ()
        else
          releaseCallback cb
  cb <- asyncCallback1 fun'
  writeIORef cbr cb
  _ <- js_requestAnimationFrame cb win
  pure $ writeIORef contr False

#ifdef GHCJS_NEW
foreign import javascript safe "((cb,win) => { return win.requestAnimationFrame(cb); })"
  js_requestAnimationFrame :: Callback (JSVal -> IO ()) -> Window -> IO Int

foreign import javascript safe "((id,win) => { win.cancelAnimationFrame(id); })"
  js_cancelAnimationFrame :: Int -> Window -> IO ()

#else
js_requestAnimationFrame :: Callback (JSVal -> IO ()) -> Window -> IO Int
js_requestAnimationFrame = undefined

js_cancelAnimationFrame :: Int -> Window -> IO ()
js_cancelAnimationFrame = undefined
#endif

