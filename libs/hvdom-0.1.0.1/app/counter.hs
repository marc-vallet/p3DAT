
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (elem)
import HVDom qualified as HV
import HVDom.DOM.Prop qualified as HV (Prop(..), buildProp)

import Control.Concurrent qualified as Conc
import Data.Foldable (for_)
import Data.Profunctor (dimap)
import Data.Text (Text, pack)
import Data.Void
import Web.Console (consoleLog)
import Web.DOM.Document qualified as DOM (Document)
import Web.DOM.Node qualified as DOM (Node, toNode, appendChild)
import Web.DOM.ParentNode qualified as DOM (QuerySelector(..), querySelector)
import Web.Event.Event qualified as DOM (EventType(..))
import Web.HTML.Common qualified as DOM (AttrName(..))
import Web.HTML.HTMLDocument qualified as DOM (toDocument, toParentNode)
import Web.HTML.Window qualified as DOM (window, document)


main :: IO ()
main = do
  consoleLog "main: start"
  win <- DOM.window
  doc <- DOM.document win
  bod <- DOM.querySelector (DOM.QuerySelector "body") (DOM.toParentNode doc)
  chan <- Conc.newChan
  for_ bod $ \body -> do
    consoleLog "main: body"
    let spec = mkSpec (DOM.toDocument doc) (Conc.writeChan chan)
    initStep <- mkInitialStep spec (DOM.toNode body) renderState 0
    let
      loop s stp = do
        act <- Conc.readChan chan
        let s' = case act of
                AddOne -> s + 1
                SubtractOne -> s - 1
        consoleLog $ "readChan: act=" <> tshow act <> ", s'=" <> tshow s'
        stp' <- HV.step stp s'
        loop s' stp'
    loop 0 initStep
  consoleLog "main: end"

type VDom action = HV.VDom [HV.Prop action] Void

mkSpec
  :: DOM.Document -> (action -> IO ())
  -> HV.VDomSpec [HV.Prop action] Void
mkSpec document emit = HV.VDomSpec
  { buildWidget = const $ absurd
  , buildAttributes = HV.buildProp emit
  , document
  }

mkInitialStep
  :: HV.VDomSpec [HV.Prop action] Void
  -> DOM.Node
  -> (state -> VDom action)
  -> state
  -> IO (HV.Step state DOM.Node)
mkInitialStep spec parent render initVal = do
  consoleLog "mkInitialStep"
  initStep <- HV.buildVDom spec (render initVal)
  _ <- DOM.appendChild (HV.extract initStep) parent
  pure $ dimap render id initStep

type State = Int

data Action = AddOne | SubtractOne
    deriving (Eq, Show)

elem :: forall a. Text -> [HV.Prop a] -> [VDom a] -> VDom a
elem n as cs = HV.Elem Nothing (HV.ElemName n) as cs

elemWithClass :: forall a. Text -> Text -> [HV.Prop a] -> [VDom a] -> VDom a
elemWithClass n cl as =
  elem n (class_ cl : as)

text :: forall a. Text -> VDom a
text a = HV.Text a

class_ :: forall a. Text -> HV.Prop a
{--
class_ val = HV.Property className_ (HV.propFromText val)
  where
    className_ :: DOM.PropName Text
    className_ = DOM.PropName "className"
--}
class_ val = HV.Attribute Nothing (DOM.AttrName "class") val

onClick :: forall a. a -> HV.Prop a
onClick act = HV.Handler (DOM.EventType "click") (const $ Just act)

renderState :: State -> VDom Action
renderState counter =
  elemWithClass "div" "container-fluid" []
    [ elemWithClass "div" "row" []
        [ elem "button" [ onClick AddOne ] [ text "+" ]
        , elem "button" [ onClick SubtractOne ] [ text "-" ]
        ]
    , elemWithClass "div" "row" []
        [ elem "div" []
            [ text $ pack $ "Counter: " <> show counter
            ]
        ]
    ]

tshow :: Show a => a -> Text
tshow = pack . show

