
{-# LANGUAGE CPP #-}

module Fluid.Html.Events
where
import           Fluid.Html.Core

import           Control.Monad ((>=>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Web.FFI.DSL as FFI
import qualified Web.Event.Event as E
import qualified Web.Event.EventTarget as E
import           Web.UIEvent.KeyboardEvent as KE (KeyboardEvent, fromEvent)
import           Web.UIEvent.MouseEvent as ME  (MouseEvent, fromEvent)
#ifdef OLD_VDOM
import           Web.MVU.Types (Phase(..), Options(..), onWithOptions)
#endif

-----------------------------------------------------------------------------

#ifdef OLD_VDOM
-- | Default value for @Options@.
--
-- > defaultOptions = Options { _preventDefault = False, _stopPropagation = False }
defaultOptions :: Options
defaultOptions =
  Options
  { _preventDefault = False
  , _stopPropagation = False
  }
#endif

-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
-- This is used to define events that are triggered during the browser
-- bubble phase.
--
on :: E.EventType
   -> (E.Event -> Maybe action)
   -> Prop action
#ifdef OLD_VDOM
on typ f = onWithOptions BUBBLE defaultOptions typ f
#else
on typ f = Handler typ f
#endif

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: (MouseEvent -> action) -> Prop action
onClick f =
  on (E.EventType "click") (fmap f . ME.fromEvent)

onMouseDown :: (MouseEvent -> action) -> Prop action
onMouseDown f = on (E.EventType "mousedown") $ fmap f . ME.fromEvent

onMouseUp :: (MouseEvent -> action) -> Prop action
onMouseUp f = on (E.EventType "mouseup") $ fmap f . ME.fromEvent

onMouseOver :: (MouseEvent -> action) -> Prop action
onMouseOver f = on (E.EventType "mouseover") $ fmap f . ME.fromEvent

onMouseOut :: (MouseEvent -> action) -> Prop action
onMouseOut f = on (E.EventType "mouseout") $ fmap f . ME.fromEvent

onKeyDown :: (KeyboardEvent -> action) -> Prop action
onKeyDown f = onKeyDown' $ Just . f

onKeyDown' :: (KeyboardEvent -> Maybe action) -> Prop action
onKeyDown' f = on (E.EventType "keydown") $ KE.fromEvent >=> f

onKeyUp :: (KeyboardEvent -> action) -> Prop action
onKeyUp f = onKeyUp' $ Just . f

onKeyUp' :: (KeyboardEvent -> Maybe action) -> Prop action
onKeyUp' f = on (E.EventType "keyup") $ KE.fromEvent >=> f

-----------------------------------------------------------------------------

-- | Event handler which gets `prop` field from
-- | the event's target.
propHandler :: String -> (FFI.JSVal -> value) -> E.Event -> Maybe value
propHandler pname preader ev = do -- Monad Maybe
    E.EventTarget t <- E.target ev
    let jp = FFI.prop pname t
    FFI.fromNullable preader jp

onValueChange :: (Text -> action) -> Prop action
onValueChange f =
  on (E.EventType "change") $
    fmap f . propHandler "value" FFI.fromJSText

onValueInput :: (Text -> action) -> Prop action
onValueInput f =
  on (E.EventType "input") $
    fmap f . propHandler "value" FFI.fromJSText

