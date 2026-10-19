
{-# LANGUAGE CPP #-}

module Fluid.Html.Elements
where
import           Fluid.Html.Core

import           Data.Text (Text)


-- | Low-level helpers used to construct 'HTML' 'node' in 'Fluid.Html.Core'.
-- Almost all functions in this module, like 'div_', 'table_' etc. are defined in terms of it.
nodeHtml :: Text -> [Prop action] -> [Html w action] -> Html w action
#ifdef OLD_VDOM
nodeHtml = elemNode HTML
#else
nodeHtml = elemNode Nothing
#endif

emptyNodeHtml :: Text -> [Prop action] -> Html w action
emptyNodeHtml nodeName = flip (nodeHtml nodeName) []


div_ :: [Prop a] -> [Html w a] -> Html w a
div_ = nodeHtml "div"

span_ :: [Prop a] -> [Html w a] -> Html w a
span_ = nodeHtml "span"

h1_ :: [Prop a] -> [Html w a] -> Html w a
h1_ = nodeHtml "h1"

h2_ :: [Prop a] -> [Html w a] -> Html w a
h2_ = nodeHtml "h2"

h3_ :: [Prop a] -> [Html w a] -> Html w a
h3_ = nodeHtml "h3"

h4_ :: [Prop a] -> [Html w a] -> Html w a
h4_ = nodeHtml "h4"

h5_ :: [Prop a] -> [Html w a] -> Html w a
h5_ = nodeHtml "h5"

h6_ :: [Prop a] -> [Html w a] -> Html w a
h6_ = nodeHtml "h6"

hr_ :: [Prop a] -> Html w a
hr_ = emptyNodeHtml "hr"

a_ :: [Prop a] -> [Html w a] -> Html w a
a_ = nodeHtml "a"

b_ :: [Prop a] -> [Html w a] -> Html w a
b_ = nodeHtml "b"

br_ :: [Prop a] -> Html w a
br_ = emptyNodeHtml "br"

-- List elements:

ul_ :: [Prop a] -> [Html w a] -> Html w a
ul_ = nodeHtml "ul"

ol_ :: [Prop a] -> [Html w a] -> Html w a
ol_ = nodeHtml "ol"

li_ :: [Prop a] -> [Html w a] -> Html w a
li_ = nodeHtml "li"

dl_ :: [Prop a] -> [Html w a] -> Html w a
dl_ = nodeHtml "dl"

dt_ :: [Prop a] -> [Html w a] -> Html w a
dt_ = nodeHtml "dt"

dd_ :: [Prop a] -> [Html w a] -> Html w a
dd_ = nodeHtml "dd"

-- Form elements:

label_ :: [Prop a] -> [Html w a] -> Html w a
label_ = nodeHtml "label"

button_ :: [Prop a] -> [Html w a] -> Html w a
button_ = nodeHtml "button"

input_ :: [Prop a] -> Html w a
input_ = emptyNodeHtml "input"

-- Table elements

table_ :: [Prop a] -> [Html w a] -> Html w a
table_ = nodeHtml "table"

thead_ :: [Prop a] -> [Html w a] -> Html w a
thead_ = nodeHtml "thead"

tbody_ :: [Prop a] -> [Html w a] -> Html w a
tbody_ = nodeHtml "tbody"

tr_ :: [Prop a] -> [Html w a] -> Html w a
tr_ = nodeHtml "tr"

th_ :: [Prop a] -> [Html w a] -> Html w a
th_ = nodeHtml "th"

td_ :: [Prop a] -> [Html w a] -> Html w a
td_ = nodeHtml "td"

