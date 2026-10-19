
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

module Fluid.Query.EvalM
where
#ifdef DIRECT_MONAD
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
#else
import           Control.Monad.Free.Church
#endif
import           Control.Monad.State.Class
import           Control.Monad.IO.Class
import           Data.Bifunctor


-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'Fluid.update' function.
type Sub action = Sink action -> IO ()

-- | Function to asynchronously dispatch actions to the 'Fluid.update' function.
type Sink action = action -> IO ()

-- | Turn a subscription that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (a -> b) -> Sub a -> Sub b
mapSub f subA sinkB =
    subA (sinkB . f)


data Schedule action output
  = SchedAct !Bool !(Sub action)
  | SchedOut output
  deriving (Functor)

instance Bifunctor Schedule where
  bimap fa _fo (SchedAct sync sub) =
    SchedAct sync (mapSub fa sub)
  bimap _fa fo (SchedOut out) =
    SchedOut (fo out)

#ifdef DIRECT_MONAD
newtype EvalM state action output a =
    EvalM (StateT state (Writer [Schedule action output]) a)
    deriving (Functor, Applicative, Monad)

deriving instance MonadState state (EvalM state action output)

-- | Schedule an IO action for later execution, which can cause some actions
-- to be dispatched to the 'eval' function.
scheduleSub' :: Bool -> Sub action -> EvalM state action output ()
scheduleSub' sync ioAction = EvalM $ tell [SchedAct sync ioAction]

-- | Raises an output message for the component.
raise :: output -> EvalM state action output ()
raise o = EvalM $ tell [SchedOut o]

#else
data EvalF state action output a
  = State (state -> (a, state))
  | Schedule (Schedule action output) a
{-
  | Subscribe (SubscriptionId -> Emitter action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | Unlift (UnliftIO (FluidM state action slots output m) -> IO a)
  | ChildQuery (CQ.ChildQuery slots a)
  | Par (FluidAp state action slots output m a)
  | Fork (FluidM state action slots output m ()) (ForkId -> a)
  | Join ForkId a
  | Kill ForkId a
  | GetRef RefLabel (Maybe Element -> a)
-}
  deriving (Functor)

newtype EvalM state action output a =
    EvalM (F (EvalF state action output) a)
    deriving (Functor, Applicative, Monad)

instance MonadState state (EvalM state action output) where
    -- state :: (state -> (a, state)) -> EvalM state action output a
    state f = EvalM $ liftF $ State f

-- | Schedule an IO action for later execution, which can cause some actions
-- to be dispatched to the 'eval' function.
scheduleSub' :: Bool -> Sub action -> EvalM state action output ()
scheduleSub' sync ioAction = EvalM $ liftF $ Schedule (SchedAct sync ioAction) ()

-- | Raises an output message for the component.
raise :: output -> EvalM state action output ()
raise o = EvalM $ liftF $ Schedule (SchedOut o) ()
#endif

-- | Schedule an IO action for later execution, which can cause some actions
-- to be dispatched to the 'eval' function.
scheduleSub :: Sub action -> EvalM state action output ()
scheduleSub ioAction = scheduleSub' False ioAction

scheduleIO :: IO action -> EvalM state action output ()
scheduleIO ioAction = scheduleSub (ioAction >>=)

-- | Like 'scheduleIO' but doesn't cause an action to be dispatched to
-- the 'eval' function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
scheduleIO_ :: IO () -> EvalM state action output ()
scheduleIO_ ioAction = scheduleSub (const ioAction)

scheduleSync :: IO action -> EvalM state action output ()
scheduleSync ioAction = scheduleSub' True (ioAction >>=)

scheduleSync_ :: IO () -> EvalM state action output ()
scheduleSync_ ioAction = scheduleSub' True (const ioAction)


mapAction :: forall state action action' output a
           . (action -> action')
          -> EvalM state action output a
          -> EvalM state action' output a
mapAction fa = mapEvalM idLens fa id

mapOutput :: forall state action output output' a
           . (output -> output')
          -> EvalM state action output a
          -> EvalM state action output' a
mapOutput fo = mapEvalM idLens id fo

mapEvalM :: forall state state' action action' output output' a
          . (state' -> (state, state -> state'))
         -> (action -> action')
         -> (output -> output')
         -> EvalM state action output a
         -> EvalM state' action' output' a
#ifdef DIRECT_MONAD
mapEvalM lens fa fo (EvalM fm) =
  EvalM $ StateT $ \s' -> do -- Monad (Writer [Schedule action' output'])
        let (s1, g) = lens s'
        (x, s2) <- mapWriter h $ runStateT fm s1
        pure (x, g s2)
  where
    h :: (p, [Schedule action output]) -> (p, [Schedule action' output'])
    h (p, scheds) = (p, fmap (bimap fa fo) scheds)
#else
mapEvalM lens fa fo (EvalM fm) =
  EvalM $ hoistF go fm
  where
    go :: forall x. EvalF state action output x -> EvalF state' action' output' x
    go (State f) = State $ \s' -> let (s, g) = lens s' in fmap g (f s)
    go (Schedule ioaction x) = Schedule (bimap fa fo ioaction) x
#endif

idLens :: forall s. s -> (s, s -> s)
idLens s = (s, id)

