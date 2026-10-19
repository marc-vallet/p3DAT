
{-# LANGUAGE CPP               #-}

module Web.DOM.ParentNode
    ( ParentNode(..), IsParentNode, toParentNode
    , QuerySelector(..)
    , querySelector
    )
where
import           Web.DOM.Internal.Types (Element(..))
import           Web.DOM.Node
import           Web.FFI.Types

import           Data.Coerce (coerce, Coercible)
import           Data.Text (Text)

newtype ParentNode = ParentNode JSVal

class IsNode o => IsParentNode o

toParentNode :: IsParentNode o => o -> ParentNode
toParentNode = ParentNode . coerce

instance IsNode ParentNode
instance IsParentNode ParentNode

---------------------------------------------------

newtype QuerySelector = QuerySelector Text

querySelector :: IsParentNode self => QuerySelector -> self -> IO (Maybe Element)
querySelector (QuerySelector qs) self =
  fromNullable Element <$> js_querySelector (mkJSStringT qs) (toParentNode self)

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((qs,self) => { return self.querySelector(qs); })"
  js_querySelector :: JSString -> ParentNode -> IO JSVal
#else
js_querySelector = undefined
#endif

