
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE CPP                #-}

module Drawing.Activity
    (
    -- * Main entry points
      drawingOf, animationOf
    , activityOf
    -- * Update
    , UpdateM, Sink
    , module Control.Monad.State.Class
    , deferIO, deferIO_, deferSub
    --- , putLog
    -- * View
    , module Drawing
    -- * Properties
    , Prop
    , pixelsByUnit
    -- ** Events
    -- *** View's resize
    , onResize
    -- *** Keyboard
    , onKeyDown, onKeyDown', onKeyUp, onKeyUp'
    -- *** Mouse
    , onMouseDown, onMouseUp
    , onMouseMove
    -- *** Time
    , onTimePassing
    , Time
    -- ** Logging
    , logging, loggingFluid
    )
where
import qualified Drawing.Activity.Element as DE

import           Drawing
import           Drawing.Vector

import           Control.Monad (when, forM_)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (StateT, execStateT)
import           Control.Monad.Writer.Strict
import           Data.IORef
import           Data.Maybe (isJust)
import           Debug.Trace
import           Fluid
import           Fluid.App
import qualified Fluid.Html as FH
import qualified Fluid.Html.Events as FE
import           Fluid.Query.Event (eventListener)
import           Fluid.Subscription (subscribe)
import           Fluid.Utils.StableNamed
import           Web.Console (consoleLog)
import           Web.Event.Event
import           Web.HTML.Window (window, toEventTarget, fromEventTarget, innerWidth, innerHeight, requestAnimationFrame)
import qualified Web.UIEvent.KeyboardEvent as KE
import qualified Web.UIEvent.MouseEvent as ME

-- ---------------------------------------------------------------
-- Main pure entry points
-- ---------------------------------------------------------------

-- | A simple drawing program.
drawingOf :: Double                     -- ^ The unit size in pixels.
          -> Drawing
          -> IO ()
drawingOf usize draw =
    activityOf [pixelsByUnit usize]
               () (const $ pure ()) (const draw)

-- | A program which shows an animation, with a drawing for each time given by the parameter.
animationOf :: Double                   -- ^ The unit size in pixels.
            -> (Time -> Drawing)        -- ^ Function to produce the next frame of animation.
                                        --   It is passed the time in seconds since the program started.
            -> IO ()
animationOf usize view =
    activityOf [pixelsByUnit usize, onTimePassing const]
               0 put view

type UpdateM model action = StateT model (Writer [Sink action -> IO ()])

type Time = Double

data Prop action
  = Logging Bool
  | PixelsByUnit Double
        -- ^ The unit size in pixels.
  | OnResize (Point -> action)
        -- ^ The resize handler.
  | OnMouseMove (Point -> action)
        -- ^ The mouse movement handler.
  | OnTimePassing (Time -> Time -> action)
        -- ^ The time passing handler. The handler is called with 2 arguments:
        --      abs: Time in seconds from the begining.
        --      rel: Time in seconds since the previous time passing.
  | ViewProp (FH.Prop (Msg action))

logging :: Prop action
logging = Logging False

loggingFluid :: Prop action
loggingFluid = Logging True

-- | The unit size in pixels.
-- By default (without this property), a drawing's unit is 1 pixel.
pixelsByUnit :: Double -> Prop action
pixelsByUnit = PixelsByUnit

-- | The resize handler.
onResize :: (Point -> action)
         -- ^ Callback receiving the view's size (width, height) in drawing's units
         -> Prop action
onResize = OnResize

-- | The mouse movement handler.
onMouseMove :: (Point -> action) -> Prop action
onMouseMove = OnMouseMove

-- | The time passing handler.
onTimePassing :: (Time -> Time -> action)
              -- ^ Callback receiving 2 arguments:
              -- the time in seconds from the begining
              -- and the time in seconds since the previous time passing
              -> Prop action
onTimePassing = OnTimePassing

-- | The keydown handler.
onKeyDown :: (Text -> Text -> action)
          -- ^ Callback receiving the key's phisical code (not affected by modifiers)
          -- and the key's value
          -> Prop action
onKeyDown f = onKeyDown' ((.) Just . f)

onKeyDown' :: (Text -> Text -> Maybe action) -> Prop action
onKeyDown' f =
  ViewProp $ FE.onKeyDown' $ \ev -> do
    kev <- KE.fromEvent ev
    AWrap <$> f (KE.code kev) (KE.key kev)

-- | The keyup handler.
onKeyUp :: (Text -> Text -> action)
        -- ^ Callback receiving the key's phisical code (not affected by modifiers)
        -- and the key's value
        -> Prop action
onKeyUp f = onKeyUp' ((.) Just . f)

onKeyUp' :: (Text -> Text -> Maybe action) -> Prop action
onKeyUp' f =
  ViewProp $ FE.onKeyUp' $ \ev -> do
    kev <- KE.fromEvent ev
    AWrap <$> f (KE.code kev) (KE.key kev)

onMouseDown :: (Point -> action) -> Prop action
onMouseDown f =
  ViewProp $ FE.onMouseDown $
    AMouseDown True f

onMouseUp :: (Point -> action) -> Prop action
onMouseUp f =
  ViewProp $ FE.onMouseUp $
    AMouseDown False f


-- | An interactive program that responds to events.
-- Activities can interact with the user, change over time, and remember information about the past.
activityOf :: [Prop action]    -- ^ The properties of the activity.
           -> model            -- ^ The initial state of the activity.
           -> (action -> UpdateM model action ())
                               -- ^ The action handling function, which updates the state given an action.
           -> (model -> Drawing)
                               -- ^ The visualization function, which converts the state into a drawing to display.
           -> IO ()
activityOf props model0 update view = do
  let env0 = Env False 1 Nothing Nothing Nothing update view []
      (opts, env) = foldl processProp (defaultOptions, env0) props
                
  runUI' (component env model0) () opts
  pure ()
  where
    processProp (opts, env) (Logging False) = (opts, env{ eLogging = True })
    processProp (opts, env) (Logging True) = (opts{ optLogs = True }, env)
    processProp (opts, env) (PixelsByUnit usz) = (opts, env{ ePixelsByUnit = usz })
    processProp (opts, env) (OnResize f) = (opts, env{ eOnResize = Just f })
    processProp (opts, env) (OnMouseMove f) = (opts, env{ eOnMouseMove = Just f })
    processProp (opts, env) (OnTimePassing f) = (opts, env{ eOnTimePassing = Just f })
    processProp (opts, env) (ViewProp p) = (opts, env{ eProps = eProps env ++ [p] })

component :: Env model action
          -> model
          -> Component input_ output_
component env model0 = mkComponent'
        ComponentSpec{ initialState = const $
                         State{ sModel = mkStableNamed model0
                              , sViewXMax = 0, sViewYMax = 0
                              , sMouseIsIn = Nothing
                              }
                     , render = viewState env
                     , eval = mkEval
                         defaultEval{ initialize = Just AInit
                                    , handleAction = updateState env
                                    }
                     }

-- | Schedule a single IO action for later execution.
deferIO :: IO action -> UpdateM model action ()
deferIO io = deferSub (io >>=)

deferIO_ :: IO () -> UpdateM model action ()
deferIO_ io = deferSub $ const io

-- | Schedule a single IO action for later execution.
deferSub :: (Sink action -> IO ()) -> UpdateM model action ()
deferSub io = tell [ io ]


-- TODO
---putLog :: Text -> IO ()
---putLog = consoleLog


splitTime :: Time -> (Time -> a -> a) -> Time -> a -> a
splitTime maxdt update dt state =
    if maxdt < dt then splitTime maxdt update (dt - maxdt) (update maxdt state)
    else update dt state


-- ---------------------------------------------------------------
-- Internal (implementation)

data Env model action = Env
        { eLogging :: Bool
        , ePixelsByUnit :: Double
        , eOnResize :: Maybe (Point -> action)
        , eOnMouseMove :: Maybe (Point -> action)
        , eOnTimePassing :: Maybe (Time -> Time -> action)
        , eUpdate :: !(action -> UpdateM model action ())
        , eVisual :: !(model -> Drawing)
        , eProps :: [FH.Prop (Msg action)]
        }

data State model
    = State
        { sModel :: !(StableNamed model)
        , sViewXMax :: !Int -- in pixels
        , sViewYMax :: !Int -- in pixels
        , sMouseIsIn :: !(Maybe (Int, Int))
        }
    deriving Eq

instance Show (State model) where
  showsPrec _ State{..} =
    showString "State{sViewXMax=" . shows sViewXMax .
      showString ",sViewYMax=" . shows sViewYMax .
      showString ",sMouseIsIn=" . shows sMouseIsIn . showChar '}'

data Msg a
  = AWrap a
  | AInit
  | AWinResize Event
  | AWinResize' Int Int
  | AMouseIn (Maybe ME.MouseEvent)  -- if mouse is in the drawing
                                    --   then it is 'Just event'
  | AMouseMov ME.MouseEvent
  | AMouseDown Bool (Point -> a) ME.MouseEvent   -- True/false iff mouse down/up
  deriving (Functor)

instance Show (Msg a) where
  showsPrec p AInit =
    showString "AInit"
  showsPrec p (AWrap _) =
    showString "AWrap _"
  showsPrec p (AWinResize _) =
    showString "AWinResize"
  showsPrec p (AWinResize' w h) = showParen (p > 10) $
    showString "AWinResize' " . showsPrec 11 w . showChar ' ' . showsPrec 11 h
  showsPrec p (AMouseIn mbe) = showParen (p > 10) $
    showString "AMouseIn " . showsPrec 11 mbe
  showsPrec p (AMouseMov e) = showParen (p > 10) $
    showString "AMouseMov " . showsPrec 11 e
  showsPrec p (AMouseDown d _ e) = showParen (p > 10) $
    showString "AMouseDown " . showsPrec 11 d . showChar ' ' . showsPrec 11 e

instance Show KE.KeyboardEvent where
  show ev =
    showString "KeyboardEvent{code=" $ shows (KE.code ev) $
      showString ",key=" $ shows (KE.key ev)  "}"

instance Show ME.MouseEvent where
  show ev =
    showString "MouseEvent{screen=" $ showCoord ME.screenX ME.screenY $
      showString ",client=" $ showCoord ME.clientX ME.clientY $
      showString ",offset=" $ showCoord ME.offsetX ME.offsetY $
      showString ",page=" $ showCoord ME.pageX ME.pageY "}"
    where
      showCoord fX fY =
        showChar '(' . shows (fX ev) . showChar ',' . shows (fY ev) . showChar ')'

mbLog :: Env model action -> String -> a -> a
mbLog env msg x =
  if eLogging env then trace msg x
  else x

mbLogIO :: Env model action
        -> String
        -> IO ()
mbLogIO env msg =
  when (eLogging env) $
    consoleLog $ pack msg

mbLogM :: Env model action
       -> String
       -> EvalM (State model) (Msg action) output ()
mbLogM env msg =
  when (eLogging env) $
    scheduleSync_ $
      consoleLog $ pack msg

---------------------------------
-- Rendering the view

viewState :: Env model action -> State model -> ComponentHtml (Msg action) _slots
viewState env state =
    mbLog env "+++ Activity.viewState" $
  FH.div_ [ FH.class_ "container-fluid" ]
    [ FH.h3_ []
        [ FH.text "Drawing.Activity" ]
    , FH.div_ [ FH.class_ "row" ] $
        [ FH.div_ [ FH.class_ "col-md" ]
            [ DE.view $ elementInput $ unStableNamed $ sModel state ]
        ]
    ]
  where
    -- elementInput :: model -> DE.Input (Msg action)
    elementInput model =
      DE.Input{ DE.pixelsByUnit = ePixelsByUnit env
              , DE.viewXMax = sViewXMax state
              , DE.viewYMax = sViewYMax state
              , DE.drawing = eVisual env model
              , DE.attributes = eProps env <> svgEvents
              }
      where
        svgEvents =
          [ FH.tabIndex_ 0
          , FE.onMouseOver $ AMouseIn . Just
          , FE.onMouseOut $ const $ AMouseIn Nothing
          ]

---------------------------------
-- Update the state

updateState :: Env model action
            -> Msg action
            -> EvalM (State model) (Msg action) output ()
updateState env a = do
    mbLogM env $ "updateState: action=" <> show a
    state1 <- get
    updateState' env a
    state2 <- get
    mbLogM env $ "    " <> show state1 <> " ==> " <> show state2

updateState' env (AWrap act) =
    handleAppEvent env act

updateState' env AInit = do
  scheduleIO $ do
    win <- window
    width <- innerWidth win
    height <- innerHeight win
    pure $ AWinResize' width height
  scheduleSub $ \sink -> do
    win <- window
    do
        let emr = eventListener (ME.EventType "resize") (toEventTarget win) $
                Just . AWinResize
        _ <- subscribe emr sink
        pure ()
    when (isJust $ eOnMouseMove env) $ do
        let emr = eventListener (ME.EventType "mousemove") (toEventTarget win) $
                \ev -> AMouseMov <$> ME.fromEvent ev
        _ <- subscribe emr sink
        pure ()
    case eOnTimePassing env of
      Just onTimePassing -> do
        starttimer <- newIORef 0
        lasttimer <- newIORef 0
        let cb timestamp = do
              --- consoleLog $ pack $ "animation frame: timestamp=" <> show timestamp
              starttime <- readIORef starttimer
              (absdelta, reldelta) <-
                if starttime == 0 then do
                  writeIORef starttimer timestamp
                  pure (0, 0)
                else do
                  lasttime <- readIORef lasttimer
                  pure (timestamp - starttime, timestamp - lasttime)
              writeIORef lasttimer timestamp
              sink $ AWrap $ onTimePassing (absdelta / 1000) (reldelta / 1000)
              pure True
        _ <- requestAnimationFrame cb win
        pure ()
      Nothing ->
        pure ()

updateState' env (AWinResize event) =
  case target event >>= fromEventTarget of
    Just win -> do
      state <- get
      scheduleSync $ do
          width <- innerWidth win
          height <- innerHeight win
          mbLogIO env $
              "AWinResize: (innerWidth/Height)=" <> show (width, height) <>
                ", (sViewXMax/YMax)=" <> show (sViewXMax state, sViewYMax state)
          pure $ AWinResize' width height
    Nothing -> pure ()

updateState' env (AWinResize' width height) = do
  state <- get
  let state' = state{sViewXMax = div width 2 - 20, sViewYMax = div height 2 - 40}
      newsize = (2 / ePixelsByUnit env) *^
        (fromIntegral $ sViewXMax state', fromIntegral $ sViewYMax state')
  put state'
  case eOnResize env of
    Just actf -> handleAppEvent env $ actf newsize
    Nothing -> pure ()
  mbLogM env $
        "AWinResize': (sViewXMax/YMax)=" <> show (sViewXMax state', sViewYMax state') <>
          ", newsize=" <> show newsize

updateState' _ (AMouseIn mbinfo) = do
    let mkDelta event =
          (ME.offsetX event - ME.clientX event, ME.offsetY event - ME.clientY event)
    modify $ \ state -> state{ sMouseIsIn = mkDelta <$> mbinfo }

updateState' env (AMouseMov event) = do
    mouseIsIn <- gets sMouseIsIn
    case mouseIsIn of
        Just offsetDelta -> do
            state <- get
            let Just actf = eOnMouseMove env
                client = (ME.clientX event, ME.clientY event)
                (offsetX, offsetY) = client ^+^ offsetDelta
                x = fromIntegral (offsetX - sViewXMax state) / ePixelsByUnit env
                y = fromIntegral (sViewYMax state - offsetY) / ePixelsByUnit env
            handleAppEvent env $ actf (x, y)
        Nothing -> pure ()

updateState' env (AMouseDown down actf event) = do
    state <- get
    let offsetX = ME.offsetX event
        offsetY = ME.offsetY event
        x = fromIntegral (offsetX - sViewXMax state) / ePixelsByUnit env
        y = fromIntegral (sViewYMax state - offsetY) / ePixelsByUnit env
    handleAppEvent env $ actf (x, y)

handleAppEvent :: Env model action
               -> action
               -> EvalM (State model) (Msg action) output ()
handleAppEvent env ev =
    mbLog env "+++ Activity.handleAppEvent" $
  do
    snmodel <- gets sModel
    let (model', ios) = runWriter $ execStateT (eUpdate env ev) $ unStableNamed snmodel
        snmodel' = mkStableNamed model'
    when (True || snmodel' /= snmodel) $
      modify $ \state -> state{ sModel = snmodel' }
    forM_ ios $ \io ->
      scheduleSub $ \sink ->
        io (sink . AWrap)

