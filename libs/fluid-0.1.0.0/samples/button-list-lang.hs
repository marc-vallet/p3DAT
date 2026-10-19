
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeFamilies       #-}

import qualified Button as Button

import           Fluid
import           Fluid.App (runUI)
import           Fluid.Console (consoleLog)
import qualified Fluid.Subscription as FS
import           Fluid.Html as FH

import           Data.List as L
import           Data.Row (type (.==))
import           Data.Text (Text, pack)


main :: IO ()
main = do
    mio <- runUI component ()
    consoleLog "button-list-lang started"
    _ <- FS.subscribe (messages mio) $ \msg ->
        consoleLog $ "Message: " <> msg
    pure ()

-- Root component

data Lang = Eng | Cat
data State = State [Id] Lang

data Action = Add | Remove Id | Click Id | ToggleLang
type Id = Int

type Slots = ("button" .== Button.Slot Id)

component :: Component input Text
component = mkComponent
    ComponentSpec
        { initialState = const $ State [] Eng
        , render = rootRender
        , eval = mkEval
                    defaultEval
                        { handleAction = rootEval
                        }
        }

rootEval :: Action -> EvalM State Action Text ()
rootEval Add =
    modify $ addId 0
    where
        addId i s@(State ids lang) =
            if elem i ids then addId (i + 1) s else State (ids ++ [i]) lang
rootEval (Remove i) =
    modify $ remId i
    where
        remId i s@(State ids lang) =
            State (L.delete i ids) lang
rootEval (Click i) =
    raise $ "Button "<>pack (show i)<>" is clicked"
rootEval ToggleLang =
    modify toggle
    where
        toggle (State ids Eng) = State ids Cat
        toggle (State ids Cat) = State ids Eng

rootRender :: State -> ComponentHtml Action Slots
rootRender (State ids lang) =
  div_ [ class_ "container-fluid" ]
    [ div_ [ class_ "row" ]
        [ button_ [ onClick $ const Add ] [ text $ alabel lang ]
        , button_ [ onClick $ const ToggleLang ] [ text $ tlabel lang ]
        ]
    , div_ [ class_ "row" ]
        (but <$> ids)
    , div_ [ class_ "row" ]
        [ div_ []
            [ text $ "Ids: " <> pack (show ids)
            ]
        ]
    , hr_ []
    ]
  where
    alabel Eng = "New button"
    alabel Cat = "Nou botó"
    tlabel Eng = "Toggle language"
    tlabel Cat = "Canvia idioma"
    blabel Eng = "Button"
    blabel Cat = "Botó"
    rlabel Eng = "Remove"
    rlabel Cat = "Elimina"
    but i =
      div_ [ class_ "row" ]
        [ slot #button i Button.component (blabel lang <> " " <> pack (show i)) (const $ Click i)
        , button_ [ onClick $ const $ Remove i ] [ text $ rlabel lang ]
        ]

