
module Web.DOM.Internal.Types
where
import           Web.FFI.Types

newtype Node = Node JSVal

newtype NodeList = NodeList JSVal

newtype Element = Element JSVal

newtype HTMLCollection = HTMLCollection JSVal

