module Web.HTML.Common
where
import Data.String
import Data.Text (Text)

newtype PropName value = PropName Text
  deriving (Eq, Ord, Show)

newtype AttrName = AttrName Text
  deriving (Eq, Ord, Show, IsString)

newtype ClassName = ClassName Text
  deriving (Eq, Ord, Show, IsString)

