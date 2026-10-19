
{-# LANGUAGE CPP        #-}

module Web.UIEvent.MouseEvent
    ( module Web.UIEvent.MouseEvent
    , module Web.UIEvent.UIEvent
    , module Web.Event.Event
    )
where
import           Web.Event.Event
import           Web.Event.Internal.Types
import           Web.UIEvent.UIEvent
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text, pack, unpack)


newtype MouseEvent = MouseEvent JSVal

newMouseEvent :: Text -> IO MouseEvent
newMouseEvent type_ =
  js_newMouseEvent (mkJSStringT type_)

---------------------------------------------------

class IsUIEvent o => IsMouseEvent o

toMouseEvent :: IsMouseEvent o => o -> MouseEvent
toMouseEvent = MouseEvent . coerce

instance IsMouseEvent MouseEvent
instance IsUIEvent MouseEvent
instance IsEvent MouseEvent

fromEvent :: IsEvent o => o -> Maybe MouseEvent
fromEvent obj =
  let Event jobj = toEvent obj
  in MouseEvent <$> unsafeReadProtoTagged "MouseEvent" jobj

---------------------------------------------------

screenX :: IsMouseEvent self => self -> Int
screenX ev =
  js_screenX (toMouseEvent ev)

screenY :: IsMouseEvent self => self -> Int
screenY ev =
  js_screenY (toMouseEvent ev)

clientX :: IsMouseEvent self => self -> Int
clientX ev =
  js_clientX (toMouseEvent ev)

clientY :: IsMouseEvent self => self -> Int
clientY ev =
  js_clientY (toMouseEvent ev)

offsetX :: IsMouseEvent self => self -> Int
offsetX ev =
  js_offsetX (toMouseEvent ev)

offsetY :: IsMouseEvent self => self -> Int
offsetY ev =
  js_offsetY (toMouseEvent ev)

pageX :: IsMouseEvent self => self -> Int
pageX ev =
  js_pageX (toMouseEvent ev)

pageY :: IsMouseEvent self => self -> Int
pageY ev =
  js_pageY (toMouseEvent ev)

button :: IsMouseEvent self => self -> Int
button ev =
  js_button (toMouseEvent ev)

relatedTarget :: IsMouseEvent self => self -> Maybe EventTarget
relatedTarget ev =
  fromNullable EventTarget $ js_relatedTarget (toMouseEvent ev)


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ) => { return new MouseEvent(typ); })"
  js_newMouseEvent :: JSString -> IO MouseEvent

foreign import javascript unsafe
  "((ev) => { return ev.screenX; })"
  js_screenX :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.screenY; })"
  js_screenY :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.clientX; })"
  js_clientX :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.clientY; })"
  js_clientY :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.offsetX; })"
  js_offsetX :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.offsetY; })"
  js_offsetY :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.pageX; })"
  js_pageX :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.pageY; })"
  js_pageY :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.button; })"
  js_button :: MouseEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.relatedTarget; })"
  js_relatedTarget :: MouseEvent -> JSVal

#else
js_newMouseEvent = undefined
js_screenX = undefined
js_screenY = undefined
js_clientX = undefined
js_clientY = undefined
js_offsetX = undefined
js_offsetY = undefined
js_pageX = undefined
js_pageY = undefined
js_button = undefined
js_relatedTarget = undefined
#endif

