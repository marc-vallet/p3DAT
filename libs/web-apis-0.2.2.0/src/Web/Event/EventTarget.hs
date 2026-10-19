
{-# LANGUAGE CPP        #-}

module Web.Event.EventTarget
    ( EventTarget(..)
    , module Web.Event.EventTarget
    )
where
import           Web.Event.Internal.Types
import           Web.Event.Event
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text, pack, unpack)


---------------------------------------------------

class Coercible o JSVal => IsEventTarget o

toEventTarget :: IsEventTarget o => o -> EventTarget
toEventTarget = EventTarget . coerce

instance IsEventTarget EventTarget

---------------------------------------------------

-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener Mozilla EventTarget.addEventListener documentation>
addEventListener :: (IsEventTarget self)
                 => EventType -> (Event -> IO ()) -> Bool
                 -> self -> IO (IO ())
addEventListener (EventType type_) callback capture self = do
  cb <- syncCallback1 ContinueAsync (callback . Event)
  let self' = toEventTarget self
      type_' = mkJSStringT type_
  js_addEventListener type_' cb capture self'
  pure $ do
    js_removeEventListener type_' cb capture self'
    releaseCallback cb

-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.dispatchEvent Mozilla EventTarget.dispatchEvent documentation>
dispatchEvent :: (IsEventTarget self, IsEvent event)
              => event
              -> self -> IO Bool
dispatchEvent event self =
  js_dispatchEvent (toEvent event) (toEventTarget self)

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ,call,capt,target) => { target.addEventListener(typ,call,capt); })"
  js_addEventListener :: JSString -> Callback (JSVal -> IO ()) -> Bool -> EventTarget -> IO ()

foreign import javascript unsafe
  "((typ,call,capt,target) => { target.removeEventListener(typ,call,capt); })"
  js_removeEventListener :: JSString -> Callback (JSVal -> IO ()) -> Bool -> EventTarget -> IO ()

foreign import javascript unsafe
  "((ev,target) => { return target.dispatchEvent(ev); })"
  js_dispatchEvent :: Event -> EventTarget -> IO Bool

#else
js_addEventListener = undefined
js_removeEventListener = undefined
js_dispatchEvent = undefined
#endif

