
{-# LANGUAGE CPP #-}

module Fluid.App.Driver.View
where
import           Fluid
import           Fluid.Html.Core

import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Void
import           Debug.Trace
#ifdef OLD_VDOM
import           Web.MVU.Types hiding (VDom(..))
import qualified Web.MVU.Types as VDom
#else
import           HVDom.Types as VDom
#endif

type ViewKey = Int

data ViewAction =
      ViewMany [ViewOp]
    | ViewIO (IO ())

data ViewOp =
      ViewAdd ViewKey (Html ViewKey ViewAction)
    | ViewRemove ViewKey

instance Show ViewOp where
    showsPrec p (ViewAdd k t) = showParen (p > 10) $
        \s -> "ViewAdd " ++ show k ++ " " ++ showHtml t ++ s
    showsPrec p (ViewRemove k) = showParen (p > 10) $
        showString "ViewRemove " . shows k


data ViewModel = ViewModel
        { vwTrees :: Map.Map ViewKey (Html ViewKey ViewAction)
        , vwRoot :: ViewKey
        }

viewInitModel :: ViewKey -> ViewModel
viewInitModel k =
    ViewModel
        { vwRoot = k
        , vwTrees = Map.empty
        }

#ifdef OLD_VDOM
viewRender :: ViewModel -> VDom.VDom ViewAction
#else
viewRender :: ViewModel -> VDom [Prop ViewAction] Void
#endif
viewRender vm =
    viewWidget (vwRoot vm)
    where
        -- viewWidget :: ViewKey -> VDom [Prop ViewAction] Void
        viewWidget key =
            case Map.lookup key (vwTrees vm) of
                Just tree ->
                    htmlToView viewWidget tree
                Nothing ->
                    trace ("viewRender: Widget with invalid key " <> show key) $
                        VDom.Text $ "[BUG: viewRender: Widget with invalid key " <> T.pack (show key) <> "]"

#ifdef OLD_VDOM
htmlToView :: (w -> VDom.VDom a) -> Html w a -> VDom.VDom a
htmlToView f (Elem ns name as children) =
    VDom.Elem ns name as $ htmlToView f <$> children
htmlToView f (Widget w) =
    f w
htmlToView _ (Text txt) =
    VDom.Text txt
#else
htmlToView :: (w -> VDom [Prop a] Void) -> Html w a -> VDom [Prop a] Void
htmlToView f (Html vdom) =
  htmlToView' vdom
  where
    htmlToView' (Elem ns name as children) =
      Elem ns name as $ htmlToView' <$> children
    htmlToView' (Widget w) =
      f w
    htmlToView' (Text txt) =
      Text txt
#endif


viewUpdate :: ViewAction -> ViewModel -> IO ViewModel
viewUpdate (ViewMany actions) m =
    pure $ foldl viewOp m actions
    where
        viewOp m (ViewAdd k t) =
            m{ vwTrees = Map.insert k t (vwTrees m) }
        viewOp m (ViewRemove k) =
            m{ vwTrees = Map.delete k (vwTrees m) }
viewUpdate (ViewIO ioaction) m = do
    ioaction
    pure m

