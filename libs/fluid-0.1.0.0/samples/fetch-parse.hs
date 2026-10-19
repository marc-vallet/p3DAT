
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main
where
import           Control.Exception
import           Data.Text (Text, pack)
import           GHC.Generics
import           Fluid
import           Fluid.App (runUI)
import           Fluid.Console
import           Fluid.Html as H
import qualified Fluid.Html.Parser as P
import           Fluid.Html.Properties as HP
--import qualified Fluid.CSS as CSS
import qualified Web.Fetch as X

-- | Main entry point
main :: IO ()
main = do
    runUI component ()
    pure ()

-- | Model
data Model
  = Model
  { apiUrl :: Text
  , apiInfo :: Maybe (Either Text Text)
  } deriving (Eq, Show)

-- | Action
data Action
  = ApiUrl Text
  | FetchApiInfo
  | SetApiInfo (Either Text Text)
  deriving (Show, Eq)

component :: Component input output
component = mkComponent
    ComponentSpec
        { initialState = const $ Model defaultApiUrl Nothing
        , render = viewModel
        , eval = mkEval
                    defaultEval
                        { handleAction = updateModel
                        }
        }

defaultApiUrl :: Text
defaultApiUrl = "https://api.github.com/"
--defaultApiUrl = "http://localhost:8008/"

-- | Update your model
updateModel :: Action -> EvalM Model Action output ()
updateModel (ApiUrl value) = do
  modify $ \ m -> m { apiUrl = value }
  scheduleIO_ $
    consoleLog $ "updateModel: ApiUrl " <> value
updateModel FetchApiInfo = do
  url <- gets apiUrl
  modify $ \ m -> m { apiInfo = Nothing }
  scheduleSub $ \sink -> do
    consoleLog "updateModel: FetchApiInfo"
    getAPIInfo url >>= sink . SetApiInfo
updateModel (SetApiInfo apiInfo) =
  modify $ \ m -> m { apiInfo = Just apiInfo }

-- | View function, with routing
viewModel :: Model -> ComponentHtml Action slots
viewModel Model {..} =
    div_ [ _CSS_style_
                    [ _CSS_textAlign "center"
                    , _CSS_margin "200px"
                    ]
         ]
      [ h1_ [class_ "title" ] [ text "Fluid Fetch Example" ]
      , div_ []
          [ label_ [] [ "Url:" ]
          , input_ [ value_ apiUrl, onValueInput ApiUrl ]
          ]
      , button_ ([ onClick $ const FetchApiInfo
                 , class_ "button is-large is-outlined"
                 ] <> if isDisabled then [ disabled_ ] else [])
          [ text $ "Fetch JSON from " <> apiUrl <> " via Fetch"
          ]
      , case apiInfo of
          Nothing -> div_ [] [ text "No data" ]
          Just (Left err) -> div_ [] [ text $ "Error: " <> err]
          Just (Right t) ->
            div_ []
              [ div_ [] [ "Text:"
                        , text t
                        ]
              , div_ [] [ "Parsed:"
                        , div_ [] $
                            case P.parseText t of
                              Left err -> [ text $ "Error: " <> err]
                              Right v ->  v
                        ]
              ]
      ]
  where
    _CSS_style_ props = HP.classes_ []
    _CSS_textAlign name = undefined
    _CSS_margin name = undefined
    disabled_ = HP.classes_ []
    isDisabled = case apiInfo of
                Just (Right _) -> True
                _ -> False

-----------------------------------------------

getAPIInfo :: Text -> IO (Either Text Text)
getAPIInfo url = do
  handle hExc $ do
    resp <- X.fetch url X.emptyRI
    Right <$> X.text resp
  where
    hExc :: SomeException -> IO (Either Text a)
    hExc = pure . Left . tshow

tshow :: Show a => a -> Text
tshow = pack . show

