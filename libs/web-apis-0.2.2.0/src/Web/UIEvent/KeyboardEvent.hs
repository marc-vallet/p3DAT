
{-# LANGUAGE CPP        #-}

module Web.UIEvent.KeyboardEvent
    ( module Web.UIEvent.KeyboardEvent
    , module Web.UIEvent.UIEvent
    , module Web.Event.Event
    )
where
import           Web.Event.Event
import           Web.Event.Internal.Types
import           Web.UIEvent.UIEvent
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text)


newtype KeyboardEvent = KeyboardEvent JSVal

newKeyboardEvent :: Text -> IO KeyboardEvent
newKeyboardEvent type_ =
  js_newKeyboardEvent (mkJSStringT type_)

---------------------------------------------------

instance IsUIEvent KeyboardEvent
instance IsEvent KeyboardEvent

fromEvent :: IsEvent o => o -> Maybe KeyboardEvent
fromEvent obj =
  let Event jobj = toEvent obj
  in KeyboardEvent <$> unsafeReadProtoTagged "KeyboardEvent" jobj


key :: KeyboardEvent -> Text
key ev =
  unJSStringT $ js_key ev

code :: KeyboardEvent -> Text
code ev =
  unJSStringT $ js_code ev

data KeyLocation
  = Standard    -- 0
  | Left        -- 1
  | Right       -- 2
  | Numpad      -- 3
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

location :: KeyboardEvent -> KeyLocation
location ev =
  toEnum $ js_location ev

ctrlKey :: KeyboardEvent -> Bool
ctrlKey ev =
  js_ctrlKey ev

shiftKey :: KeyboardEvent -> Bool
shiftKey ev =
  js_shiftKey ev

altKey :: KeyboardEvent -> Bool
altKey ev =
  js_altKey ev

metaKey :: KeyboardEvent -> Bool
metaKey ev =
  js_metaKey ev


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ) => { return new KeyboardEvent(typ); })"
  js_newKeyboardEvent :: JSString -> IO KeyboardEvent

foreign import javascript unsafe
  "((ev) => { return ev.key; })"
  js_key :: KeyboardEvent -> JSString

foreign import javascript unsafe
  "((ev) => { return ev.code; })"
  js_code :: KeyboardEvent -> JSString

foreign import javascript unsafe
  "((ev) => { return ev.location; })"
  js_location :: KeyboardEvent -> Int

foreign import javascript unsafe
  "((ev) => { return ev.ctrlKey; })"
  js_ctrlKey :: KeyboardEvent -> Bool

foreign import javascript unsafe
  "((ev) => { return ev.shiftKey; })"
  js_shiftKey :: KeyboardEvent -> Bool

foreign import javascript unsafe
  "((ev) => { return ev.altKey; })"
  js_altKey :: KeyboardEvent -> Bool

foreign import javascript unsafe
  "((ev) => { return ev.metaKey; })"
  js_metaKey :: KeyboardEvent -> Bool

#else
js_newKeyboardEvent = undefined
js_key = undefined
js_code = undefined
js_location = undefined
js_ctrlKey = undefined
js_shiftKey = undefined
js_altKey = undefined
js_metaKey = undefined
#endif

