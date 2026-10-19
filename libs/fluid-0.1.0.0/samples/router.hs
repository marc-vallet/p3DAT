
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
where
import           Fluid
import           Fluid.App (runUI)
import           Fluid.Console
import           Fluid.Html
import           Fluid.Html.Properties
import           Fluid.Router
import           Fluid.Subscription

instance Router Route where
  --fromRoute :: Route -> LocalURI
  fromRoute r = LocalURI segs [] Nothing
    where
      segs = case r of
        Home -> ["index.html"]
        About -> ["about.html"]

  --toRoute :: LocalURI -> Maybe Route
  toRoute luri =
    case luriPath luri of
      ["index.html"] -> Just Home
      ["about.html"] -> Just About
      _ -> Nothing

---------------------------------------------

-- | Main entry point
main :: IO ()
main = do
    runUI component ()
    pure ()

data Route = Home | About
  deriving (Show, Eq)

component :: Component input output
component = mkComponent
    ComponentSpec
        { initialState = const Nothing
        , render = viewModel
        , eval = mkEval
                    defaultEval
                        { handleAction = logAndHandle updateModel
                        , initialize = Just Initialize
                        }
        }

-- | Update your model
logAndHandle :: Show action
             => (action -> EvalM state action output a)
             -> action -> EvalM state action output a
logAndHandle handleAction act = do
  scheduleSync_ $
    consoleLog $ "handleAction: action=" <> pack (show act)
  handleAction act

-- | Action
data Action
  = Initialize
  | SetRoute Route
  | RouterError LocalURI
  | PushRoute Route
  deriving (Show, Eq)

updateModel :: Action -> EvalM (Maybe Route) Action output ()
updateModel Initialize =
    scheduleSub $ \sink -> do
      -- Get actual local URI
      luri <- getLocalURI
      sink $ maybe (RouterError luri) SetRoute $ toRoute luri
      -- Register  a listener for local URI changes
      emr <- routeListener SetRoute RouterError
      _ <- subscribe emr sink
      pure ()
updateModel (SetRoute newRoute) =
    put $ Just newRoute
updateModel (RouterError luri) = do
    scheduleIO_ $
      consoleError $ "Invalid URI " <> localURIToText luri
    put Nothing
updateModel (PushRoute route) =
    scheduleIO_ $
      pushRoute route

-- | View function, with routing
viewModel :: Maybe Route -> ComponentHtml Action slots
viewModel v = div_ []
  [ h1_ []
      [ "miso2-router" ]
  , h2_ []
      [ case v of
        Just Home -> home
        Just About -> about
        Nothing -> the404
      ]
  ]
  where
      home =
        div_ []
          [ div_ [ ] [text "home"]
          , button_ [onClick $ const goAbout] [text "go about"]
          ]
      about =
        div_ []
          [ div_ [] [text "about"]
          , button_ [onClick $ const goHome] [text "go home"]
          ]
      the404 =
        div_ []
          [ text "the 404 :("
          , button_ [onClick $ const goHome] [text "go home"]
          , button_ [onClick $ const goAbout] [text "go about"]
          ]

-- | Type-level routes

goHome, goAbout :: Action
goHome = PushRoute Home
goAbout = PushRoute About

