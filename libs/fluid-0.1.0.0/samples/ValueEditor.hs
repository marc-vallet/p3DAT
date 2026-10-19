
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE NamedFieldPuns          #-}

module ValueEditor
where
import Fluid as F
import Fluid.Html

import Data.Text (Text, pack)

-- Value editor component

type Slot v = F.Slot v v

data State v = State { sValue :: v }
    deriving Eq

data Action = Change Text

component :: (Text -> Maybe v)
          -> (v -> Text)
          -> Component v v
component toValue fromValue = mkComponent
    ComponentSpec
        { initialState = \v -> State v
        , render = render_ fromValue
        , eval = mkEval
                    defaultEval
                        { receive = const Nothing --Just . Change . fromValue
                        , handleAction = handleAction_ toValue
                        }
        }

render_ :: (v -> Text) -> State v -> ComponentHtml Action slots
render_ fromValue s =
    input_ [ onValueChange Change, value_ (fromValue $ sValue s) ]

handleAction_ :: (Text -> Maybe v) -> Action -> EvalM (State v) Action v ()
handleAction_ toValue (Change val) = do
    case toValue val of
        Just val' -> do
            modify $ \s -> s{ sValue = val' }
            raise val'
        Nothing -> pure ()

