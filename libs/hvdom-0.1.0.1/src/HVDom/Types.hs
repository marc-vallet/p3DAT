
module HVDom.Types
  ( VDom (..)
  , renderWidget
  , Graft (..)
  , runGraft
  , ElemName (..)
  , unElemName
  , Namespace (..)
  , unNamespace
  )
where
import Data.Bifunctor
import Data.String
import Data.Text (Text)

newtype ElemName = ElemName Text
  deriving (Eq, Ord, Show, IsString)

unElemName :: ElemName -> Text
unElemName (ElemName n) = n

newtype Namespace = Namespace Text
  deriving (Eq, Ord, Show, IsString)

unNamespace :: Namespace -> Text
unNamespace (Namespace ns) = ns

data VDom as w
  = Text Text
  | Elem (Maybe Namespace) ElemName as [VDom as w]
  | Keyed (Maybe Namespace) ElemName as [(Text, VDom as w)]
  | Widget w
  | Grafted (Graft as w)
  deriving (Functor)

instance Bifunctor VDom where
  bimap f g = \case
    Text s -> Text s
    Elem ns'm en props children -> Elem ns'm en (f props) (map (bimap f g) children)
    Keyed ns'm en props children -> Keyed ns'm en (f props) (map (second (bimap f g)) children)
    Widget w -> Widget $ g w
    Grafted graft -> Grafted $ bimap f g graft

data Graft as w = forall as' w'. Graft (as' -> as) (w' -> w) (VDom as' w')

deriving instance Functor (Graft as)

instance Bifunctor Graft where
  bimap f g (Graft fm wm v) = Graft (f . fm) (g . wm) v

runGraft :: Graft as w -> VDom as w
runGraft (Graft fm fw v) = bimap fm fw v

renderWidget :: (as -> as') -> (w -> VDom as' w') -> VDom as w -> VDom as' w'
renderWidget fm injWidget = \case
  Text txt -> Text txt
  Elem ns'm en props children ->
    Elem ns'm en (fm props) (map (renderWidget fm injWidget) children)
  Keyed ns'm en props children ->
    Keyed ns'm en (fm props) (map (second (renderWidget fm injWidget)) children)
  Widget w -> injWidget w
  Grafted graft -> renderWidget fm injWidget (runGraft graft)

