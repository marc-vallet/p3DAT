
{-# LANGUAGE CPP        #-}

module Web.UIEvent.PointerEvent
    ( module Web.UIEvent.PointerEvent
    , module Web.UIEvent.MouseEvent
    , module Web.UIEvent.UIEvent
    , module Web.Event.Event
    )
where
import           Web.Event.Event
import           Web.UIEvent.MouseEvent
import           Web.UIEvent.UIEvent
import           Web.FFI.Types

import           Data.Text (Text)


newtype PointerEvent = PointerEvent JSVal

newPointerEvent :: Text -> IO PointerEvent
newPointerEvent type_ =
  js_newPointerEvent (mkJSStringT type_)

---------------------------------------------------

instance IsMouseEvent PointerEvent
instance IsUIEvent PointerEvent
instance IsEvent PointerEvent


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ) => { return new PointerEvent(typ); })"
  js_newPointerEvent :: JSString -> IO PointerEvent

#else
js_newPointerEvent = undefined
#endif

