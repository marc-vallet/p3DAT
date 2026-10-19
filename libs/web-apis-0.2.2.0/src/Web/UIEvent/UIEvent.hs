
{-# LANGUAGE CPP        #-}

module Web.UIEvent.UIEvent
    ( module Web.UIEvent.UIEvent
    , module Web.Event.Event
    )
where
import           Web.Event.Event
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text)


newtype UIEvent = UIEvent JSVal

newUIEvent :: Text -> IO UIEvent
newUIEvent type_ =
  js_newUIEvent (mkJSStringT type_)

---------------------------------------------------

class IsEvent o => IsUIEvent o

toUIEvent :: IsUIEvent o => o -> UIEvent
toUIEvent = UIEvent . coerce

instance IsUIEvent UIEvent
instance IsEvent UIEvent


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ) => { return new UIEvent(typ); })"
  js_newUIEvent :: JSString -> IO UIEvent

#else
js_newUIEvent = undefined
#endif

