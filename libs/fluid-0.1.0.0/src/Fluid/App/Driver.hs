
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP #-}

module Fluid.App.Driver
where
import           Fluid
import           Fluid.App.Driver.Model
import           Fluid.App.Driver.View
import           Fluid.Component
import           Fluid.Html as FH
import           Fluid.Query
import           Fluid.Utils.Conc
import           Fluid.Utils.Log
import           Fluid.Utils.Slot as Slot
import           Fluid.Utils.StableNamed

#ifdef DIRECT_MONAD
#else
import           Control.Monad.Free.Church
#endif
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import qualified Data.DList as DL


data RunEffects = RunEffects !(DL.DList (IO ())) !(DL.DList ViewOp)

conViewOp :: ViewOp -> RunEffects -> RunEffects
conViewOp va (RunEffects ios vas) =
    RunEffects ios (DL.cons va vas)

instance Semigroup RunEffects where
    RunEffects ios1 vas1 <> RunEffects ios2 vas2 =
        RunEffects (ios1 <> ios2) (vas1 <> vas2)

instance Monoid RunEffects where
    mempty =
        RunEffects DL.empty DL.empty


handleQueryIO :: DriverModel state action slots input output
              -> Sink ViewAction
              -> FluidQ action input
              -> IO ()
handleQueryIO dm vsink q = do
    RunEffects ios vas <- atomicallyTracing $ runComponent q dm vsink
    if null ios then
        logMsg "no IO actions"
    else do
        logMsg $ "sequence of " ++ show (length ios) ++ " IO actions"
        vsink $ ViewIO $ sequence_ ios
    if null vas then
        logMsg "no view actions"
    else do
        logMsg $ show (length vas) ++ " view actions:" ++
                foldMap (\va s -> "\n        " ++ shows va s) vas ""
        vsink $ ViewMany $ DL.toList vas

runComponent :: forall state action slots input output.
                FluidQ action input
             -> DriverModel state action slots input output
             -> Sink ViewAction
             -> TraceSTM RunEffects
runComponent q dm vsink = do
    (dirty, sio) <- runEval q dm vsink
    let dirty' = case q of
            Initialize -> True
            Finalize -> False
            _ -> dirty
    if dirty' then do
        logMsg $ "Render component "++show (dmKey dm)
        vas <- runRender dm vsink
        pure $ RunEffects (maybe DL.empty DL.singleton sio) DL.empty <> vas
    else
        pure $ RunEffects (maybe DL.empty DL.singleton sio) DL.empty

runEval :: forall state action slots input output.
           FluidQ action input
        -> DriverModel state action slots input output
        -> Sink ViewAction
        -> TraceSTM (Bool, Maybe (IO ()))
runEval q dm vsink = do
        (n, s) <- liftSTM $ (,) <$> readTVar (dmSeqNum dm) <*> readTVar (dmState dm)
        logMsg $ "EvalM component "++show (dmKey dm)++":"++show n++": "++showFluidQ q
        let evalM = eval (dmComponent dm) q
            x = unStableNamed s
            (x', scheds) = runEvalM' evalM x
            s' = mkStableNamed x'
        liftSTM $ do
            writeTVar (dmSeqNum dm) (n + 1)   
            writeTVar (dmState dm) s'   
        dirty <-
            if s == s' then
                pure False
            else
                case dmEq dm of
                    Just eq -> do
                        let dirty = not $ eq x x'
                            msg = if dirty then "sn1 /= sn2 and x1 /= x2 (dirty)"
                                           else "sn1 /= sn2 but x1 == x2 (not dirty) !!!"
                        logMsg msg
                        pure dirty
                    Nothing -> pure True
        let io = if null scheds then Nothing
                 else Just $ mapM_ doSchedule scheds
        pure (dirty, io)
    where
        runEvalM' :: EvalM state action output () -> state -> (state, [Schedule action output])
        runEvalM' m x =
            let ((_, x'), scheds) = runWriter $ runStateT (runEvalM m) x
            in (x', scheds)
        runEvalM :: EvalM state action output () -> StateT state (Writer [Schedule action output]) ()
#ifdef DIRECT_MONAD
        runEvalM (EvalM m) = m
#else
        runEvalM (EvalM m) = foldF step m

        step :: EvalF state action output x -> StateT state (Writer [Schedule action output]) x
        step (State f) =
            state f
        step (Schedule sched x) = do
            lift $ tell [sched]
            pure x
#endif
        doSchedule :: Schedule action output -> IO ()
        doSchedule (SchedAct False sub) = do
            forkJSM (sub ahandler)
            pure ()
        doSchedule (SchedAct True sub) =
            sub ahandler
        doSchedule (SchedOut out) =
            ohandler out

        ohandler :: output -> IO ()
        ohandler = dmOutputHandler dm
        ahandler :: action -> IO ()
        ahandler = handleQueryIO dm vsink . Action


runRender :: forall state action slots input output.
             DriverModel state action slots input output
          -> Sink ViewAction
          -> TraceSTM RunEffects
runRender dm vsink = do
    s <- liftSTM $ readTVar $ dmState dm
    let comp = dmComponent dm
        html = render comp (unStableNamed s)
    childrenInRef <- liftSTM $ readTVar (dmChildren dm) >>= newTVar
    childrenOutRef <- liftSTM $ newTVar Slot.empty
    (vtree, res1) <- runWriterT $ do
        vtree <- traverseWidget (convWidget childrenInRef childrenOutRef) html
        -- vtree :: Html ViewKey action
        pure $ (ViewIO . handleQueryIO dm vsink . Action) <$> vtree
    -- vtree :: Html ViewKey ViewAction
    liftSTM $ readTVar childrenOutRef >>= writeTVar (dmChildren dm)
    -- Remove slots in childrenInRef
    childrenIn <- liftSTM $ readTVar childrenInRef
    res2 <- removeChildren childrenIn vsink
    let va = ViewAdd (dmKey dm) vtree
    pure $ conViewOp va (res1 <> res2)
    where
        convWidget :: TVar (SlotStorage slots DriverModelX)
                   -> TVar (SlotStorage slots DriverModelX)
                   -> ComponentSlot slots action
                   -> WriterT RunEffects TraceSTM ViewKey
        convWidget childrenInRef childrenOutRef slot = do
            (ckey, res) <- lift $ convWidget' childrenInRef childrenOutRef slot
            tell res
            pure ckey
        convWidget' childrenInRef childrenOutRef (ComponentSlot{..}) = do
            -- sComponent :: Component cinput coutput
            -- sInput :: cinput
            -- sOutput :: coutput -> Maybe action
            children <- liftSTM $ readTVar childrenInRef
            (cdmx, ckey, res) <- case spop children of
                Nothing -> do
                    unComponent sComponent $ \mbeq spec -> do
                        -- spec :: ComponentSpec cs' cact' cps' cinput coutput
                        let chandler cout = case sOutput cout of
                                Nothing -> pure ()
                                Just act -> handleQueryIO dm vsink $ Action act
                        cdm <- liftSTM $ initDriverModel (dmFreshKey dm) mbeq spec sInput chandler
                        res <- runComponent Initialize cdm vsink
                        pure (DriverModelX cdm, dmKey cdm, res)
                Just (cdmx, children') -> do
                    liftSTM $ writeTVar childrenInRef children'
                    -- cdmx :: DriverModelX cinput coutput
                    unDriverModelX cdmx $ \cdm -> do
                        -- cdm :: DriverModel cs' cact' cps' cinput coutput
                        res <- runComponent (Receive sInput) cdm vsink
                        pure (cdmx, dmKey cdm, res)
            liftSTM $ modifyTVar' childrenOutRef $ sset cdmx
            pure (ckey, res)

removeChildren :: forall slots.
                  SlotStorage slots DriverModelX
               -> Sink ViewAction
               -> TraceSTM RunEffects
removeChildren children vsink = do
    execWriterT $ Slot.foreachSlot children $ \cdmx ->
        -- TODO: Remove cdmx
        unDriverModelX cdmx $ \cdm -> do
            res <- lift (removeChild cdm vsink)
            tell res

removeChild :: forall state action slots input output.
               DriverModel state action slots input output
            -> Sink ViewAction
            -> TraceSTM RunEffects
removeChild dm vsink = do
    -- 1st: Remove the view
    let res1 = RunEffects DL.empty (DL.singleton $ ViewRemove (dmKey dm))
    -- 2nd: Remove the children recursively
    children <- liftSTM $ do
        children <- readTVar (dmChildren dm)
        writeTVar (dmChildren dm) Slot.empty
        pure children
    res2 <- removeChildren children vsink
    -- 3rd: Eval the finalizer
    res3 <- runComponent Finalize dm vsink
    pure $ res1 <> res2 <> res3

