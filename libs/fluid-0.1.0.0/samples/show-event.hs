
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE CPP                #-}

module Main where
import           Fluid
import           Fluid.App
import qualified Fluid.Html as H
import qualified Fluid.Html.Events as HE
import           Fluid.Subscription (subscribe)
import qualified Web.Event.Event as E

-----------------------------------------------------

main :: IO ()
main = do
  runUI' component () defaultOptions{ optLogs = True }
  pure ()

type State = (Int, [(Int, EvDesc)])

data Action
  = Init
  | Push EvDesc
  | Delete Int
    deriving (Show)

component = mkComponent
        ComponentSpec{ initialState = const (1, [])
                     , render = viewState
                     , eval = mkEval
                         defaultEval{ initialize = Just Init
                                    , handleAction = handleEvent
                                    }
                     }

handleEvent :: Action -> EvalM State Action output ()
handleEvent Init =
    scheduleSub $ \sink -> do
        -- _ <- subscribe (keyboardSub Keyboard) sink
        pure ()
handleEvent (Push desc) =
    modify $ \(next, map) -> (next+1, (next, desc) : map)
handleEvent (Delete i) =
    modify $ \(next, map) -> (next, filter ((i /=) . fst) map)

viewState :: State -> Html w Action
viewState (_, descs) =
  H.div_ [ H.class_ "container-fluid" ] $
    H.h3_ [] [ H.text "miso2-show-event" ]
    : H.input_ evAttrs
    : (showEvent <$> descs)
  where
    showEvent (i, (typ, lines)) =
      let sh (k, v) =
            H.tr_ []
                [ H.td_ [] [ H.text $ pack $ show k ]
                , H.td_ [] [ H.text $ pack $ show v ]
                ]
      in H.div_ [ H.class_ "row" ]
           [ H.h4_ []
                [ H.text ("Event "<> pack (show i) <> ": " <> E.unEventType typ)
                , H.button_ [ HE.onClick $ const $ Delete i ] [ H.text "Delete" ]
                ]
           , H.table_ [] (sh <$> lines)
           ]
    evAttrs =
        [ HE.onMouseDown (Push . eventDesc)
        --, HE.on' "mouseup"   showEventDecoder Push
        --, HE.on' "mouseover" showEventDecoder Push
        --, HE.on' "mouseout"  showEventDecoder Push
        , HE.onKeyDown (Push . eventDesc)
        , HE.on (E.EventType "input") (Just . Push . eventDesc)
        , HE.onClick (Push . eventDesc)
        ]

-----------------------------------------------------------------

type EvDesc = (E.EventType, [(Text, Text)])

eventDesc :: E.IsEvent e => e -> EvDesc
eventDesc ev =
  (E.type_ ev, [])

