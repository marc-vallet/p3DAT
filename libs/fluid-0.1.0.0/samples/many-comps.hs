
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}

import qualified Button as Button
import qualified ValueEditor as VEdit

import           Fluid
import           Fluid.App (runUI)
import           Fluid.Console (consoleLog)
import qualified Fluid.Subscription as FS
import           Fluid.Html as FH

import           Data.Row (type (.==), type (.+))
import           Data.Text (Text, pack, unpack)
import           Text.Read (readMaybe)


main :: IO ()
main = do
    mio <- runUI component ()
    _ <- FS.subscribe (messages mio) $ \msg ->
        consoleLog $ "Message: " <> pack (show msg)
    pure ()

-- Root component

type State = Int

data Action = AddOne | SubtractOne | Set Int

type Slots = ("button" .== Button.Slot Int) .+ ("editor" .== VEdit.Slot Int ())

component :: Component input Text
component = mkComponent
    ComponentSpec
        { initialState = const 0
        , render = rootRender
        , eval = mkEval
                    defaultEval
                        { handleAction = rootEval
                        }
        }

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
    , slot #editor () (VEdit.component toValue fromValue) counter Set
    ]
  where
    toValue = readMaybe . unpack
    fromValue = pack . show

rootEval :: Action -> EvalM State Action Text ()
rootEval AddOne = do
    modify (+ 1)
    raise "Button + is clicked"
rootEval SubtractOne = do
    modify (subtract 1)
    raise "Button - is clicked"
rootEval (Set n) = do
    put n
    raise "Value is changed"

