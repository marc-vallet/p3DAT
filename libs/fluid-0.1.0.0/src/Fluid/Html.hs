
module Fluid.Html
   ( ComponentHtml
   , slot, slot_
   , mapComponentHtmlAction
   , module Fluid.Html.Core
   , module Fluid.Html.Elements
   , module Fluid.Html.Events
   , module Fluid.Html.Properties
   )
where
import Fluid.Component
import Fluid.Html.Core
import Fluid.Html.Elements
import Fluid.Html.Properties -- hiding (form_)
import Fluid.Html.Events

import Fluid.Utils.Slot (Slot)

import Data.Bifunctor
import Data.Row (HasType, Row, KnownSymbol, Label)


-- | A convenience synonym for the output type of a `render` function for a
-- | component that renders HTML.
-- |
-- | - `action` is the type of actions, events internal to the component that can
-- |   be evaluated with the `handleAction` function
-- | - `slots` is the set of child component types that can be used in the HTML
-- | - `m` is the monad used by the child component during evaluation
type ComponentHtml action slots = Html (ComponentSlot slots action) action


slot :: (HasType sym (Slot cinput coutput slot) slots, KnownSymbol sym, Ord slot)
     => Label sym -> slot
     -> Component cinput coutput -> cinput -> (coutput -> action)
     -> Html (ComponentSlot slots action) action
slot slabel sindex comp input omap =
    let cslot = componentSlot slabel sindex comp input (Just . omap)
    in widget cslot

slot_ :: (HasType sym (Slot cinput coutput slot) slots, KnownSymbol sym, Ord slot)
      => Label sym -> slot
      -> Component cinput coutput -> cinput
      -> Html (ComponentSlot slots action) action
slot_ slabel sindex comp input =
    let cslot = componentSlot slabel sindex comp input (const Nothing)
    in widget cslot

mapComponentHtmlAction :: (action -> action') -> ComponentHtml action slots -> ComponentHtml action' slots
mapComponentHtmlAction f =
  bimap (fmap f) f

