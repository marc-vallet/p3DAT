
module Web.HTML.HTMLDocument
  ( module Web.HTML.HTMLDocument
  , module Web.DOM.Document
  , module Web.DOM.ParentNode
  , module Web.DOM.Node
  )
where
import           Web.DOM.Document
import           Web.DOM.ParentNode
import           Web.DOM.Node
import           Web.FFI.Types

import           Data.Coerce (coerce)
import           Data.Text (Text)


newtype HTMLDocument = HTMLDocument JSVal

instance IsNode HTMLDocument
instance IsParentNode HTMLDocument
instance IsDocument HTMLDocument

