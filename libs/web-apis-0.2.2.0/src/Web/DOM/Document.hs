
{-# LANGUAGE CPP               #-}

module Web.DOM.Document
    ( Document(..), IsDocument, toDocument
    , createTextNode
    , createElement, createElementNS
    )
where
import           Web.DOM.Element
import           Web.DOM.Node
import           Web.DOM.ParentNode
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text)

newtype Document = Document JSVal

class IsParentNode o => IsDocument o

toDocument :: IsDocument o => o -> Document
toDocument = Document . coerce

instance IsNode Document
instance IsParentNode Document
instance IsDocument Document


---------------------------------------------------

createTextNode :: IsDocument self => Text -> self -> IO Node
createTextNode txt self =
  js_createTextNode (toJSText txt) (toDocument self)
{-# INLINE createTextNode #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((txt,self) => { return self.createTextNode(txt); })"
  js_createTextNode :: JSVal -> Document -> IO Node
#else
js_createTextNode = undefined
#endif

createElement :: IsDocument self => Text -> self -> IO Element
createElement name self =
  js_createElement (toJSText name) (toDocument self)
{-# INLINE createElement #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((name,self) => { return self.createElement(name); })"
  js_createElement :: JSVal -> Document -> IO Element
#else
js_createElement = undefined
#endif

createElementNS :: IsDocument self => Maybe Text -> Text -> self -> IO Element
createElementNS ns name self =
  js_createElementNS (maybe jsNull toJSText ns) (toJSText name) (toDocument self)
{-# INLINE createElementNS #-}

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((ns,name,self) => { return self.createElementNS(ns,name); })"
  js_createElementNS :: JSVal -> JSVal -> Document -> IO Element
#else
js_createElementNS = undefined
#endif

