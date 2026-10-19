
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE NamedFieldPuns          #-}

module Button
where
import Fluid as F
import Fluid.Html

import Data.Text (Text, pack)

-- Button component

type Input = Text

type Output = ()

type Slot = F.Slot Input Output

data State = State { slabel :: Text, sclicks :: Int }
    deriving Eq

data Action = SetLabel Text
            | Click

component :: Component Input Output
component = mkComponent
    ComponentSpec
        { initialState = \label -> State label 0
        , render
        , eval = mkEval
                    defaultEval
                        { receive = Just . SetLabel
                        , handleAction
                        }
        }
    where
        handleAction :: Action -> EvalM State Action Output ()
        handleAction (SetLabel label) = do
            modify $ \s -> s{ slabel = label }
        handleAction Click = do
            modify $ \s -> s{ sclicks = sclicks s + 1 }
            raise ()

        render :: State -> ComponentHtml Action slots
        render s =
          button_ [ onClick $ const Click ] [ text (slabel s <> " (" <> pack (show $ sclicks s) <> ")") ]

