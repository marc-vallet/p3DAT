
module Fluid.Html.Properties
where
import           Fluid.Html.Core

import           Data.Text (Text)
import qualified Data.Text as T

class_ :: Text -> Prop action
class_ = textProp "class"

-- | Define multiple classes
--
-- > div_ [ classes_ [ "red", "warning" ] ] []
--
classes_ :: [Text] -> Prop action
classes_ =
  textProp "class" . T.unwords

href_ :: Text -> Prop action
href_ = textProp "href"

id_ :: Text -> Prop action
id_ = textProp "id"

for_ :: Text -> Prop action
for_ = textProp "for"

type_ :: Text -> Prop action
type_ = textProp "type"

start_ :: Text -> Prop action
start_ = textProp "start"

name_ :: Text -> Prop action
name_ = textProp "name"

value_ :: Text -> Prop action
value_ = textProp "value"

min_ :: Text -> Prop action
min_ = textProp "min"

max_ :: Text -> Prop action
max_ = textProp "max"


tabIndex_ :: Int -> Prop action
tabIndex_ = intProp "tabindex"

