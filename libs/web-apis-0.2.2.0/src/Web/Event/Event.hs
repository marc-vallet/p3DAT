
{-# LANGUAGE CPP        #-}

module Web.Event.Event
where
import           Web.Event.Internal.Types
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text)


newtype Event = Event JSVal

-- | The type of strings used for event types.
newtype EventType = EventType{ unEventType :: Text }
  deriving (Eq, Ord, Show)

newEvent :: EventType -> IO Event
newEvent (EventType ty) =
  js_newEvent (mkJSStringT ty)

---------------------------------------------------

class Coercible o JSVal => IsEvent o

toEvent :: IsEvent o => o -> Event
toEvent = Event . coerce

instance IsEvent Event


---------------------------------------------------

-- | The event type.
type_ :: IsEvent self => self -> EventType
type_ ev =
  EventType $ unJSStringT $ js_type (toEvent ev)

-- | The element that was the source of the event.
target :: IsEvent self => self -> Maybe EventTarget
target ev =
  fromNullable EventTarget $ js_target (toEvent ev)

-- | The element that the event listener was added to.
currentTarget :: IsEvent self => self -> Maybe EventTarget
currentTarget ev =
  fromNullable EventTarget $ js_currentTarget (toEvent ev)


-- | Prevents the event from bubbling up to futher event listeners. Other event
-- | listeners on the current target will still fire.
stopPropagation :: IsEvent self => self -> IO ()
stopPropagation ev =
  js_stopPropagation (toEvent ev)

-- | Prevents all other listeners for the event from being called. This includes
-- | event listeners added to the current target after the current listener.
stopImmediatePropagation :: IsEvent self => self -> IO ()
stopImmediatePropagation ev =
  js_stopImmediatePropagation (toEvent ev)

-- | Indicates whether the event will bubble up through the DOM or not.
bubbles :: IsEvent self => self -> Bool
bubbles ev =
  js_bubbles (toEvent ev)

-- | Indicates whether the event can be cancelled.
cancelable :: IsEvent self => self -> Bool
cancelable ev =
  js_cancelable (toEvent ev)

-- | Cancels the event if it can be cancelled.
preventDefault :: IsEvent self => self -> IO ()
preventDefault ev =
  js_preventDefault (toEvent ev)

-- | Indicates whether `preventDefault` was called on the event.
defaultPrevented :: IsEvent self => self -> IO Bool
defaultPrevented ev =
  js_defaultPrevented (toEvent ev)


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((typ) => { return new Event(typ); })"
  js_newEvent :: JSString -> IO Event

foreign import javascript unsafe
  "((ev) => { return ev.type; })"
  js_type :: Event -> JSString

foreign import javascript unsafe
  "((ev) => { return ev.target; })"
  js_target :: Event -> JSVal

foreign import javascript unsafe
  "((ev) => { return ev.currentTarget; })"
  js_currentTarget :: Event -> JSVal

foreign import javascript unsafe
  "((ev) => { ev.stopPropagation(); })"
  js_stopPropagation :: Event -> IO ()

foreign import javascript unsafe
  "((ev) => { ev.stopImmediatePropagation(); })"
  js_stopImmediatePropagation :: Event -> IO ()

foreign import javascript unsafe
  "((ev) => { return ev.bubbles; })"
  js_bubbles :: Event -> Bool

foreign import javascript unsafe
  "((ev) => { return ev.cancelable; })"
  js_cancelable :: Event -> Bool

foreign import javascript unsafe
  "((ev) => { ev.preventDefault(); })"
  js_preventDefault :: Event -> IO ()

foreign import javascript unsafe
  "((ev) => { return ev.defaultPrevented(); })"
  js_defaultPrevented :: Event -> IO Bool

#else
js_newEvent = undefined
js_type = undefined
js_target = undefined
js_currentTarget = undefined
js_stopPropagation = undefined
js_stopImmediatePropagation = undefined
js_bubbles = undefined
js_cancelable = undefined
js_preventDefault = undefined
js_defaultPrevented = undefined
#endif

