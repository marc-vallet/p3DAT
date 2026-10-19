
{-# LANGUAGE CPP               #-}

module Web.DOM.Element
  ( module Exports
  , IsElement, toElement
  , fromParentNode, fromNode, fromEventTarget
  , setAttribute, setAttributeNS
  , removeAttribute, removeAttributeNS
  )
where
import           Web.DOM.Internal.Types
import           Web.DOM.Internal.Types as Exports (Element(..))
import           Web.DOM.ParentNode
import           Web.DOM.Node
import           Web.Event.EventTarget
import           Web.FFI.Types

import           Data.Coerce (coerce)
import           Data.Text (Text)


class IsParentNode o => IsElement o

toElement :: IsElement o => o -> Element
toElement = Element . coerce

instance IsElement Element
instance IsParentNode Element
instance IsNode Element
instance IsEventTarget Element

fromParentNode :: IsParentNode o => o -> Maybe Element
fromParentNode obj =
  let ParentNode jobj = toParentNode obj
  in Element <$> unsafeReadProtoTagged "Element" jobj
{-# INLINE fromParentNode #-}

fromNode :: IsNode o => o -> Maybe Element
fromNode obj =
  let Node jobj = toNode obj
  in Element <$> unsafeReadProtoTagged "Element" jobj
{-# INLINE fromNode #-}

fromEventTarget :: IsEventTarget o => o -> Maybe Element
fromEventTarget obj =
  let EventTarget jobj = toEventTarget obj
  in Element <$> unsafeReadProtoTagged "Element" jobj
{-# INLINE fromEventTarget #-}

---------------------------------------------------

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.hasAttribute Mozilla Element.hasAttribute documentation>
hasAttribute :: IsElement self => Text -> self -> IO Bool
hasAttribute name self =
  js_hasAttribute (mkJSStringT name) (toElement self)
{-# INLINE hasAttribute #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((n,v,o) => { return o.hasAttribute(n); })"
  js_hasAttribute :: JSString -> Element -> IO Bool
#else
js_hasAttribute = undefined
#endif

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.hasAttributeNS Mozilla Element.hasAttributeNS documentation>
hasAttributeNS :: IsElement self => Text -> Text -> self -> IO Bool
hasAttributeNS ns name self =
  js_hasAttributeNS (mkJSStringT ns) (mkJSStringT name) (toElement self)
{-# INLINE hasAttributeNS #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((ns,nm,o) => { return o.hasAttributeNS(ns,nm); })"
  js_hasAttributeNS :: JSString -> JSString -> Element -> IO Bool
#else
js_hasAttributeNS = undefined
#endif

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute :: IsElement self => Text -> Text -> self -> IO ()
setAttribute name val self =
  js_setAttribute (mkJSStringT name) (mkJSStringT val) (toElement self)
{-# INLINE setAttribute #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((n,v,o) => { o.setAttribute(n,v); })"
  js_setAttribute :: JSString -> JSString -> Element -> IO ()
#else
js_setAttribute = undefined
#endif

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttributeNS Mozilla Element.setAttributeNS documentation>
setAttributeNS :: IsElement self => Text -> Text -> Text -> self -> IO ()
setAttributeNS ns name val self =
  js_setAttributeNS (mkJSStringT ns) (mkJSStringT name) (mkJSStringT val) (toElement self)
{-# INLINE setAttributeNS #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((ns,nm,v,o) => { o.setAttributeNS(ns,nm,v); })"
  js_setAttributeNS :: JSString -> JSString -> JSString -> Element -> IO ()
#else
js_setAttributeNS = undefined
#endif

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.removeAttribute Mozilla Element.removeAttribute documentation>
removeAttribute :: IsElement self => Text -> self -> IO ()
removeAttribute name self =
  js_removeAttribute (mkJSStringT name) (toElement self)
{-# INLINE removeAttribute #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((n,o) => { o.removeAttribute(n); })"
  js_removeAttribute :: JSString -> Element -> IO ()
#else
js_removeAttribute = undefined
#endif

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.removeAttributeNS Mozilla Element.removeAttributeNS documentation>
removeAttributeNS :: IsElement self => Text -> Text -> self -> IO ()
removeAttributeNS ns name self =
  js_removeAttributeNS (mkJSStringT ns) (mkJSStringT name) (toElement self)
{-# INLINE removeAttributeNS #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((ns,nm,o) => { o.removeAttributeNS(ns,nm); })"
  js_removeAttributeNS :: JSString -> JSString -> Element -> IO ()
#else
js_removeAttributeNS = undefined
#endif

