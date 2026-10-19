
module Fluid.Query.Event where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.Text (Text)
import Fluid.Subscription as S
import Web.Event.Event as E
import Web.Event.EventTarget as E

-- | Constructs an `Emitter` for a DOM event. Accepts a function that maps event
-- | values to a `Maybe`-wrapped action, allowing it to filter events if
-- | necessary.
eventListener
  :: E.EventType
  -> E.EventTarget
  -> (E.Event -> Maybe a)
  -> S.Emitter a
eventListener eventType target f =
  eventListener' eventType target $ pure . f

eventListener'
  :: E.EventType
  -> E.EventTarget
  -> (E.Event -> IO (Maybe a))
  -> S.Emitter a
eventListener' eventType target f =
  S.makeEmitter $ \push -> do
    let listener :: E.Event -> IO ()
        listener ev = f ev >>= traverse_ push
    E.addEventListener eventType listener False target

