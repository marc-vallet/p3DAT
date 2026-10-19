
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}

module Main where
import           Fluid
import           Fluid.App as F
import           Fluid.Html as H
import           Fluid.Query.Event (eventListener)
import           Fluid.Subscription (subscribe)

import           Control.Applicative
import qualified Data.Map as M
import           Data.Text (Text)
import           Web.HTML.Window
import           Web.UIEvent.KeyboardEvent

-----------------------------------------------------

main :: IO ()
main = do
    runUI' component () F.defaultOptions{ optLogs = True }
    pure ()

component = mkComponent
        ComponentSpec{ initialState = const M.empty
                     , render = viewState
                     , eval = mkEval
                         defaultEval{ initialize = Just Init
                                    , handleAction = handleEvent
                                    }
                     }

data Action
  = Init
  | Keyboard Bool KeyboardEvent
    --deriving (Show)

handleEvent :: Action -> EvalM (M.Map Text Text) Action output ()
handleEvent Init =
    scheduleSub $ \sink -> do
        -- _ <- subscribe (keyboardSub Keyboard) sink
        win <- window
        let emtr1 = eventListener (EventType "keydown") (toEventTarget win)
                (fmap (Keyboard True) . fromEvent)
            emtr2 = eventListener (EventType "keyup") (toEventTarget win)
                (fmap (Keyboard False) . fromEvent)
        _ <- subscribe (emtr1 <|> emtr2) sink
        pure ()
handleEvent (Keyboard down kev) =
    modify $ if down then M.insert (code kev) (key kev)
                     else M.delete (code kev)

viewState :: M.Map Text Text -> ComponentHtml action slots
viewState keys =
  div_ [ class_ "container-fluid" ]
    [ h3_ []
        [ "miso2-sub" ]
    , div_ [ class_ "row" ]
        [ div_ [ class_ "col-md" ]
            [ "Press N" ]
        , div_ [ class_ "col-md" ]
            [ H.text print ]
        ]
    ]
  where
    print = if M.lookup "KeyN" keys == Just "N"
                then "[***] " <> pack (show keys)
                else "[...] " <> pack (show keys)

