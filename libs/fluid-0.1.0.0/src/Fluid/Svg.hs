
{-# LANGUAGE CPP #-}

module Fluid.Svg
where
import Fluid.Html.Core

import Data.Text (Text)

-- | Used to construct an element with namespace "svg"
elemSvg :: Text -> [Prop a] -> [Html w a] -> Html w a
elemSvg =
#ifdef OLD_VDOM
  elemNode SVG
#else
  elemNode (Just svgNS)

svgNS :: Namespace
svgNS = "http://www.w3.org/2000/svg"

#endif

