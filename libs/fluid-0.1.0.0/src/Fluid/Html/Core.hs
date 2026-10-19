
{-# LANGUAGE CPP #-}

module Fluid.Html.Core
    ( module Fluid.Html.Core
    , Namespace(..)
    , Prop(..)
    )
where
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.List as L
import           Data.String
import           Data.Text (Text, pack, unpack)
import           Web.HTML.Common as HTML
#ifdef OLD_VDOM
import           Web.MVU.Types hiding (VDom(..))
import qualified Web.MVU.Types as VDom
#else
import           HVDom.DOM.Prop as VDom
import           HVDom.Types as VDom
#endif


#ifdef OLD_VDOM
data Html w a
  = Elem Namespace Text [Prop a] [Html w a]
  | Widget w
  | Text Text
  deriving Functor

instance Bifunctor Html where
  bimap f g (Elem ns nm ps cs) =
    Elem ns nm (fmap (fmap g) ps) (fmap (bimap f g) cs)
  bimap f _g (Widget w) =
    Widget (f w)
  bimap _f _g (Text t) =
    Text t

#else
newtype Html w a = Html {unHtml :: VDom [Prop a] w}

instance Functor (Html w) where
  fmap f (Html vdom) = Html $ bimap (fmap $ fmap f) id vdom

instance Bifunctor Html where
  bimap f g (Html vdom) = Html $ bimap (fmap $ fmap g) f vdom
#endif

#ifdef OLD_VDOM
elemNode :: Namespace -> Text -> [Prop a] -> [Html w a] -> Html w a
elemNode =
  Elem
#else
elemNode :: Maybe Namespace -> Text -> [Prop a] -> [Html w a] -> Html w a
elemNode ns tag props children =
  Html $ Elem ns (ElemName tag) props (fmap unHtml children)
#endif

widget :: w -> Html w a
widget =
#ifdef OLD_VDOM
  Widget
#else
  Html . Widget
#endif

text :: Text -> Html w a
text =
#ifdef OLD_VDOM
  Text
#else
  Html . Text
#endif

instance IsString (Html w a) where
    fromString = text . pack


traverseWidget :: Applicative m => (w1 -> m w2) -> Html w1 a -> m (Html w2 a)
#ifdef OLD_VDOM
traverseWidget f = traverseWidget' (fmap Widget . f)
#else
traverseWidget f = traverseWidget' (fmap (Html . Widget) . f)
#endif

traverseWidget' :: Applicative m => (w1 -> m (Html w2 a)) -> Html w1 a -> m (Html w2 a)
#ifdef OLD_VDOM
traverseWidget' f =
    go
    where
        --go :: Applicative m => Html w1 a -> m (Html w2 a)
        go (Elem ns name as children) =
            Elem ns name as <$> traverse go children
        go (Widget w) =
            f w
        go (Text txt) =
            pure $ Text txt

#else
traverseWidget' f =
    fmap Html . go . unHtml
    where
        --go :: Applicative m => VDom [Prop a] w1 -> m (VDom [Prop a] w2)
        go (Elem ns name as children) =
            Elem ns name as <$> traverse go children
        go (Widget w) =
            fmap unHtml $ f w
        go (Text txt) =
            pure $ Text txt
#endif

data ShowMode = ShowOnlyWidgets | ShowElems | ShowNodes
    deriving (Eq, Ord)

showHtml :: Show w => Html w a -> String
showHtml = showHtml' ShowOnlyWidgets show

showHtml' :: ShowMode -> (w -> String) -> Html w a -> String
#ifdef OLD_VDOM
showHtml' mode f =
  go
  where
    -- go :: Html w a -> String
    go (Elem ns name as children) =
      let cs = go <$> children
          pred c = c /= "" && c /= "[]"
          cs' = L.filter pred cs
          el = if mode == ShowOnlyWidgets then ""
               else "<" ++ unpack name ++ ">"
      in el ++ "[" ++ L.intercalate "," cs' ++ "]"
    go (Widget w) =
      f w
    go (Text txt) | mode <= ShowElems =
      ""
    go (Text txt) =
      show txt
#else
showHtml' mode f =
  go . unHtml
  where
    -- go :: VDom [Prop a] w -> String
    go (Elem ns (ElemName name) as children) =
      let cs = go <$> children
          pred c = c /= "" && c /= "[]"
          cs' = L.filter pred cs
          el = if mode == ShowOnlyWidgets then ""
               else "<" ++ unpack name ++ ">"
      in el ++ "[" ++ L.intercalate "," cs' ++ "]"
    go (Widget w) =
      f w
    go (Text txt) | mode <= ShowElems =
      ""
    go (Text txt) =
      show txt
#endif


textProp ::  Text -> Text -> Prop action
#ifdef OLD_VDOM
textProp = Attribute
#else
textProp = Attribute Nothing . AttrName
#endif

intProp ::  Text -> Int -> Prop action
intProp n = textProp n . pack . show

