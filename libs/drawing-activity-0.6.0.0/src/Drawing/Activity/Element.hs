
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveFunctor      #-}

module Drawing.Activity.Element
where
import           Drawing.Types
import           Drawing.Render (renderSvgTree, SvgTree(..))

import           Fluid
import           Fluid.Html
import           Fluid.Svg (elemSvg)

-- ---------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------

data Input a
    = Input
        { pixelsByUnit :: !Double
        , viewXMax :: !Int -- in pixels
        , viewYMax :: !Int -- in pixels
        , drawing :: !Drawing
        , attributes :: [Prop a] -- suplementari attributes
        }
    deriving (Functor)

---------------------------------
-- Rendering the view

view :: Input a -> Html w a
view Input{..} =
    let svgTree = renderSvgTree pixelsByUnit viewXMax viewYMax drawing
    in svgToSvgView svgTree
  where
    -- svgToSvgView :: SvgTree -> Html w a
    svgToSvgView (SvgTree "svg" attrs children) =
        let svgattrs = (uncurry textProp <$> attrs) <> attributes
        in elemSvg "svg" svgattrs (toSvgView' <$> children)
        where
            toSvgView' (SvgTree name attrs children) =
                elemSvg name (uncurry textProp <$> attrs) (toSvgView' <$> children)
            toSvgView' (SvgText txt) =
                text txt

