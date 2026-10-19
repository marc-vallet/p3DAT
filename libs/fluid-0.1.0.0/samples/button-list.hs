
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeFamilies       #-}

import qualified Button as Button

import           Fluid
import           Fluid.App (runUI)
import qualified Fluid.Subscription as FS
import           Fluid.Html as FH
import           Fluid.Console

import           Data.List as L
import           Data.Row (type (.==))
import           Data.Text (pack)


main :: IO ()
main = do
    mio <- runUI component ()
    _ <- FS.subscribe (messages mio) $ \msg ->
        consoleLog $ "Message: " <> pack (show msg)
    pure ()

-- Root component

type State = [Id]

data Action = Add | Remove Id | Click Id
type Id = Int

type Slots = ("button" .== Button.Slot Id)

component :: Component input String
component = mkComponent
    ComponentSpec
        { initialState = const []
        , render = rootRender
        , eval = mkEval
                    defaultEval
                        { handleAction = rootEval
                        }
        }

rootEval :: Action -> EvalM State Action String ()
rootEval Add =
    modify $ addId 0
rootEval (Remove i) =
    modify $ L.delete i
rootEval (Click i) =
    raise $ "Button "<>show i<>" is clicked"

addId i ids =
    if elem i ids then addId (i + 1) ids else ids ++ [i]

rootRender :: State -> ComponentHtml Action Slots
rootRender ids =
  div_ [ class_ "container-fluid" ]
    [ div_ [ class_ "row" ]
        [ button_ [ onClick $ const Add ] [ text "Add" ]
        ]
    , div_ [ class_ "row" ]
        (but <$> ids)
    , div_ [ class_ "row" ]
        [ div_ []
            [ text $ "Button ids: "<>(pack $ show ids)
            ]
        ]
    , hr_ []
    ]
  where
    but i =
      div_ [ class_ "row" ]
        [ slot #button i Button.component ("Button "<>(pack $ show i)) (const $ Click i)
        , button_ [ onClick $ const $ Remove i ] [ text "Remove" ]
        ]

