
{-# LANGUAGE OverloadedStrings          #-}

import           Fluid
import           Fluid.App (runUI)
import qualified Fluid.Subscription as FS
import           Fluid.Html as FH
import           Data.Text (Text, pack)


main :: IO ()
main = do
    runUI component ()
{-
    _ <- FS.subscribe (messages mio) $ \msg ->
        consoleLog $ "Message: " <> pack (show msg)
-}
    pure ()

type State = Int

data Action = AddOne | SubtractOne

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

rootRender :: State -> ComponentHtml Action slots
rootRender counter =
  div_ [ class_ "container-fluid" ]
    [ div_ [ class_ "row" ]
        [ button_ [ onClick $ const AddOne ] [ text "+" ]
        , button_ [ onClick $ const SubtractOne ] [ text "-" ]
        ]
    , div_ [ class_ "row" ]
        [ div_ []
            [ text "Counter: "
            , text (pack $ show counter)
            ]
        ]
    ]

