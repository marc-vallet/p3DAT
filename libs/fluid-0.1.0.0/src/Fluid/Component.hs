
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}

module Fluid.Component
where
import           Fluid.Html.Core
import           Fluid.Query.FluidQ
import           Fluid.Query.EvalM

import           Fluid.Utils.Slot as Slot

import           Data.Foldable (traverse_)
import           Data.List (sort)
import           Data.Row (HasType, Row, KnownSymbol, Label)
import qualified Data.Text as T

import           Control.Monad (when)

-- ---------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------

data ComponentSpec state action slots input output = ComponentSpec
  { initialState :: input -> state
  , render :: state -> Html (ComponentSlot slots action) action
  , eval :: FluidQ action input -> EvalM state action output ()
  }

data Component input output =
    forall state action slots.
        Component (Maybe (state -> state -> Bool))
                  (ComponentSpec state action slots input output)

mkComponent :: ComponentSpec state action slots input output
            -> Component input output
mkComponent spec = Component Nothing spec

mkComponent' :: Eq state
             => ComponentSpec state action slots input output
             -> Component input output
mkComponent' spec = Component (Just (==)) spec

unComponent :: Component input output
            -> (forall state action slots.
                     Maybe (state -> state -> Bool)
                  -> ComponentSpec state action slots input output
                  -> a)
            -> a
unComponent (Component mbeq spec) k = k mbeq spec


data EvalSpec state action input output = EvalSpec
  { handleAction :: action -> EvalM state action output ()
  , receive :: input -> Maybe action
  , initialize :: Maybe action
  , finalize :: Maybe action
  }

-- | A default value for `mkEval` that will result in an `eval` that nothing at
-- | all - all incoming actions and queries will be ignored, and no receiver,
-- | initializer, or finalizer will be specified.
-- |
-- | Usually this will be used with record update syntax to override fields to
-- | specify things as needed. If a component only needs to handle actions,
-- | for instance, a usage might be something like this:
-- |
-- | ```purescript
-- | H.mkComponent
-- |   { initialState
-- |   , render
-- |   , eval: H.mkEval (H.defaultEval { handleAction = ?handleAction })
-- |   }
-- | ```
defaultEval :: EvalSpec state action input output
defaultEval =
  EvalSpec
    { handleAction = const (pure ())
    , receive = const Nothing
    , initialize = Nothing
    , finalize = Nothing
    }

-- | Accepts an `EvalSpec` to produce an `eval` function for a component. For
-- | example:
-- |
-- | ```purescript
-- | -- use `defaultEval` and override fields selectively
-- | H.mkEval (H.defaultEval { handleAction = ?handleAction })
-- |
-- | -- or specify all the fields in the `EvalSpec`
-- | H.mkEval
-- |   { handleAction: ?handleAction
-- |   , handleQuery: ?handleQuery
-- |   , receive: ?receive
-- |   , initialize: ?initialize
-- |   , finalize: ?finalize
-- |   }
-- | ```
mkEval :: EvalSpec state action input output
       -> FluidQ action input
       -> EvalM state action output ()
mkEval args q = case q of
  Initialize -> traverse_ (handleAction args) (initialize args)
  Finalize ->   traverse_ (handleAction args) (finalize args)
  Receive i ->  traverse_ (handleAction args) (receive args i)
  Action action -> handleAction args action


data ComponentSlot slots action =
    forall cinput coutput. ComponentSlot
        { sget :: forall slot. SlotStorage slots slot -> Maybe (slot cinput coutput)
        , spop :: forall slot. SlotStorage slots slot -> Maybe (slot cinput coutput, SlotStorage slots slot)
        , sset :: forall slot. slot cinput coutput -> SlotStorage slots slot -> SlotStorage slots slot
        , sComponent :: Component cinput coutput
        , sInput :: cinput
        , sOutput :: coutput -> Maybe action
        }

instance Functor (ComponentSlot slots) where
    fmap f (ComponentSlot{..}) = ComponentSlot
        { sget = sget
        , spop = spop
        , sset = sset
        , sComponent = sComponent
        , sInput = sInput
        , sOutput = fmap f . sOutput
        }

-- | Constructs a ComponentSlot
-- |
-- | Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to an action in the parent
componentSlot :: (HasType sym (Slot cinput coutput slot) slots, KnownSymbol sym, Ord slot)
              => Label sym -> slot
              -> Component cinput coutput -> cinput -> (coutput -> Maybe action)
              -> ComponentSlot slots action
componentSlot l k component input outputf =
  ComponentSlot
    { sget = Slot.lookup l k
    , spop = Slot.pop l k
    , sset = Slot.insert l k
    , sComponent = component
    , sInput = input
    , sOutput = outputf
    }

