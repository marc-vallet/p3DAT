
{-# LANGUAGE CPP               #-}

module Web.DOM.Node
    ( module Exports
    , IsNode, toNode
    , isSameNode
    , nodeName, textContent
    , parentNode, nextSibling
    , setTextContent
    , insertBefore, appendChild, replaceChild, removeChild
    )
where
import           Web.DOM.Internal.Types
import           Web.DOM.Internal.Types as Exports (Node(..))
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text)

class Coercible o JSVal => IsNode o

toNode :: IsNode o => o -> Node
toNode = Node . coerce

instance IsNode Node

---------------------------------------------------

isSameNode :: IsNode self => Node -> self -> Bool
isSameNode (Node jother) self =
  let Node jnode = toNode self
  in refEq jother jnode
{-# INLINE isSameNode #-}

nodeName :: IsNode self => self -> IO Text
nodeName self =
  unJSStringT <$> js_nodeName (toNode self)
{-# INLINE nodeName #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((self) => { return self.nodeName; })"
  js_nodeName :: Node -> IO JSString
#else
js_nodeName = undefined
#endif

textContent :: IsNode self => self -> IO (Maybe Text)
textContent self =
  fromNullable fromJSText <$> js_textContent (toNode self)
{-# INLINE textContent #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((self) => { return self.textContent; })"
  js_textContent :: Node -> IO JSVal
#else
js_textContent = undefined
#endif

parentNode :: IsNode self => self -> IO (Maybe Node)
parentNode self =
  fromNullable Node <$> js_parentNode (toNode self)
{-# INLINE parentNode #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((self) => { return self.parentNode; })"
  js_parentNode :: Node -> IO JSVal
#else
js_parentNode = undefined
#endif

nextSibling :: IsNode self => self -> IO (Maybe Node)
nextSibling self =
  fromNullable Node <$> js_nextSibling (toNode self)
{-# INLINE nextSibling #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((self) => { return self.nextSibling; })"
  js_nextSibling :: Node -> IO JSVal
#else
js_nextSibling = undefined
#endif

setTextContent :: IsNode self => Text -> self -> IO ()
setTextContent txt self =
  js_setTextContent (mkJSStringT txt) (toNode self)
{-# INLINE setTextContent #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((c,self) => { self.textContent = c; })"
  js_setTextContent :: JSString -> Node -> IO ()
#else
js_setTextContent = undefined
#endif

insertBefore :: IsNode self => Node -> Maybe Node -> self -> IO ()
insertBefore newNode ref self = do
  let jref = case ref of
        Just (Node j) -> j
        Nothing -> jsNull
  js_insertBefore newNode jref (toNode self)
{-# INLINE insertBefore #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((n,ref,self) => { self.insertBefore(n,ref); })"
  js_insertBefore :: Node -> JSVal -> Node -> IO ()
#else
js_insertBefore = undefined
#endif

appendChild :: IsNode self => Node -> self -> IO ()
appendChild child self =
  js_appendChild child (toNode self)
{-# INLINE appendChild #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((c,self) => { self.appendChild(c); })"
  js_appendChild :: Node -> Node -> IO ()
#else
js_appendChild = undefined
#endif

replaceChild :: IsNode self => Node -> Node -> self -> IO ()
replaceChild newChild oldChild self =
  js_replaceChild newChild oldChild (toNode self)
{-# INLINE replaceChild #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((n,o,self) => { self.replaceChild(n,o); })"
  js_replaceChild :: Node -> Node -> Node -> IO ()
#else
js_replaceChild = undefined
#endif

removeChild :: IsNode self => Node -> self -> IO ()
removeChild child self =
  js_removeChild child (toNode self)
{-# INLINE removeChild #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((c,self) => { self.removeChild(c); })"
  js_removeChild :: Node -> Node -> IO ()
#else
js_removeChild = undefined
#endif

