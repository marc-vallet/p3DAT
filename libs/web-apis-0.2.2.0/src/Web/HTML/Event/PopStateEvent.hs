
{-# LANGUAGE CPP        #-}

module Web.HTML.Event.PopStateEvent
where
import           Web.Event.Event
import           Web.FFI.Types

import           Data.Text (Text)


newtype PopStateEvent = PopStateEvent JSVal

newPopStateEvent :: Text -> IO PopStateEvent
newPopStateEvent type_ =
  js_newPopStateEvent (mkJSStringT type_)

---------------------------------------------------

instance IsEvent PopStateEvent


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ) => { return new PopStateEvent(typ); })"
  js_newPopStateEvent :: JSString -> IO PopStateEvent

#else
js_newPopStateEvent = undefined
#endif

