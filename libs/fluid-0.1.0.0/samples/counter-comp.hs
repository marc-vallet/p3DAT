
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}

import qualified Button as Button

import           Fluid
import           Fluid.App (runUI)
import qualified Fluid.Subscription as FS
import           Fluid.Html as FH
import           Fluid.Console

import           Data.Row (type (.==))
import           Data.Text (pack)


main :: IO ()
main = do
    mio <- runUI component ()
    _ <- FS.subscribe (messages mio) $ \msg ->
        consoleLog $ "Message: " <> pack (show msg)
    pure ()

-- Root component

type State = Int

data Action = AddOne | SubtractOne

type Slots = ("button" .== Button.Slot Int)

component :: Component input String
component = mkComponent
    ComponentSpec
        { initialState = const 0
        , render = rootRender
        , eval = mkEval
                    defaultEval
                        { handleAction = rootEval
                        }
        }

rootEval :: Action -> EvalM State Action String ()
rootEval AddOne = do
    modify (+ 1)
    raise "Button + is clicked"
rootEval SubtractOne = do
    modify (subtract 1)
    raise "Button - is clicked"

rootRender :: State -> ComponentHtml Action Slots
rootRender counter =
  div_ [ class_ "container-fluid" ]
    [ div_ [ class_ "row" ]
        [ slot #button 0 Button.component "+" (const AddOne)
        , slot #button 1 Button.component "-" (const SubtractOne)
        ]
    , div_ [ class_ "row" ]
        [ div_ []
            [ text "Counter: "
            , text (pack $ show counter)
            ]
        ]
    ]

