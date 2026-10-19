
{-# LANGUAGE CPP #-}

module Fluid.App
where
import           Fluid
import           Fluid.App.Driver
import           Fluid.App.Driver.Model
import           Fluid.App.Driver.View
import           Fluid.Component
import           Fluid.Query
import qualified Fluid.Subscription as FS
import           Fluid.Utils.Log (LogOptions, setLogOptions, logMsg, atomicallyTracing)

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exc
import           Control.Monad (when)
import qualified Data.IORef as IOR
import           Data.Profunctor (dimap)
import           Data.Void
import           Web.Console (consoleLog)
import qualified Web.DOM.Document as DOM (IsDocument, toDocument)
import qualified Web.DOM.Node as DOM
import qualified Web.DOM.ParentNode as DOM (toParentNode, QuerySelector(..), querySelector)
import qualified Web.HTML.Window as DOM (window, document, requestAnimationFrame)
#ifdef OLD_VDOM
import qualified Web.MVU.App as VDom
import qualified Web.MVU.Machine as VDom
import qualified Web.MVU.Types as VDom
#else
import qualified HVDom.DOM as VDom
import qualified HVDom.DOM.Prop as VDom
import qualified HVDom.Machine as VDom
import qualified HVDom.Types as VDom
#endif


-- ---------------------------------------------------------------
-- Main entry points
-- ---------------------------------------------------------------

runUI :: Component input output -> input -> IO (FluidIO output)
runUI comp input = runUI' comp input defaultOptions

data Options = Options{ optLogs :: LogOptions }

defaultOptions :: Options
defaultOptions = Options
        { optLogs = False
        }

runUI' :: Component input output -> input -> Options -> IO (FluidIO output)
runUI' (Component mbeq spec) input opts = do
  setLogOptions $ optLogs opts
  freshRef <- STM.newTVarIO 0
  let freshKey = fresh id freshRef
  sio <- FS.create
  dm <- STM.atomically $
    initDriverModel freshKey mbeq spec input (FS.notify $ FS.listener sio)
  qref <- newQueue
  let vmodel = viewInitModel (dmKey dm)
      notifyAction a = putQueue qref a
  handleQueryIO dm notifyAction Initialize
  win <- DOM.window
  doc <- DOM.document win
  Just parent <- DOM.querySelector (DOM.QuerySelector "body") (DOM.toParentNode doc)
#ifdef OLD_VDOM
  initStep <- mkInitialStep notifyAction vmodel
#else
  initStep <- mkInitialStep doc notifyAction vmodel
  _ <- DOM.appendChild (VDom.extract initStep) parent
#endif
  Conc.forkIO $ do
    e <- Exc.try $ scheduler parent qref vmodel initStep
    consoleLog $ "scheduler: end: " <> tshow (e :: Either Exc.SomeException ())
  pure FluidIO{ messages = FS.emitter sio
              , dispose = pure ()
              }

#ifdef OLD_VDOM
mkInitialStep :: (ViewAction -> IO ())
              -> VDom.Machine ViewModel ()
mkInitialStep emit vmodel = do
  initStep <- VDom.mkMachine emit (viewRender vmodel)
  pure $ dimap viewRender id initStep
#else
mkInitialStep :: DOM.IsDocument doc
              => doc -> (ViewAction -> IO ())
              -> VDom.Machine ViewModel DOM.Node
mkInitialStep doc emit vmodel = do
  let spec = VDom.VDomSpec
        { buildWidget = const $ absurd
        , buildAttributes = VDom.buildProp emit
        , document = DOM.toDocument doc
        }
  initStep <- VDom.buildVDom spec (viewRender vmodel)
  pure $ dimap viewRender id initStep
#endif

#ifdef OLD_VDOM
scheduler :: DOM.IsNode par
          => par -> QRef ViewAction
          -> ViewModel
          -> VDom.Step ViewModel ()
          -> IO a
scheduler _parent qref s0 stp0 = do
  waitFrame <- frameWaiter
  let
    loop s stp = do
      acts <- takeQueue qref
      s' <- viewUpdates acts s
      _timestamp <- waitFrame -- !!!
      stp' <- VDom.step stp s'
      loop s' stp'
  loop s0 stp0
#else
scheduler :: DOM.IsNode par
          => par -> QRef ViewAction
          -> ViewModel
          -> VDom.Step ViewModel DOM.Node
          -> IO a
scheduler parent qref s0 stp0 = do
  waitFrame <- frameWaiter
  let
    loop s stp = do
      let oldnode = VDom.extract stp
      acts <- takeQueue qref
      s' <- viewUpdates acts s
      _timestamp <- waitFrame -- !!!
      stp' <- VDom.step stp s'
      let newnode = VDom.extract stp'
      when (not $ DOM.isSameNode oldnode newnode) $
        DOM.appendChild newnode parent
      loop s' stp'
  loop s0 stp0
#endif

frameWaiter :: IO (IO Double)
frameWaiter = do
  frame <- Conc.newEmptyMVar :: IO (Conc.MVar Double)
  win <- DOM.window
  let rAFCallback timestamp = do
        Conc.putMVar frame timestamp
        pure False
  pure $ do
        _frameCancel <- DOM.requestAnimationFrame rAFCallback win
        Conc.takeMVar frame

viewUpdates :: [ViewAction] -> ViewModel -> IO ViewModel
viewUpdates (act : acts) s = do
      viewUpdate act s >>= viewUpdates acts
viewUpdates [] s =
      pure s

-- ---------------------------------------------------------------

{--
type QRef a = Conc.Chan a

newQueue :: IO (QRef a)
newQueue =
  Conc.newChan

putQueue :: QRef a -> a -> IO ()
putQueue qr x =
  Conc.writeChan qr x

takeQueue :: QRef a -> IO [a]
takeQueue qr =
  (: []) <$> Conc.readChan qr
--}

data QRef a = QRef !(Conc.MVar ()) !(IOR.IORef [a])

newQueue :: IO (QRef a)
newQueue =
  QRef <$> Conc.newEmptyMVar <*> IOR.newIORef []

putQueue :: QRef a -> a -> IO ()
putQueue (QRef waiter ref) x = do
  IOR.atomicModifyIORef' ref $ \xs -> (x : xs, ())
  _ <- Conc.tryPutMVar waiter ()
  pure ()

takeQueue :: QRef a -> IO [a]
takeQueue qr@(QRef waiter ref) = do
  xs <- IOR.atomicModifyIORef' ref $ \xs -> ([], xs)
  case xs of
    [] -> do
      Conc.takeMVar waiter
      takeQueue qr
    _ ->
      pure $ reverse xs

-- ---------------------------------------------------------------

fresh :: (Int -> a) -> STM.TVar Int -> STM.STM a
fresh f ref = do
    i <- STM.readTVar ref
    STM.modifyTVar' ref (1 +)
    pure $ f i

tshow :: Show a => a -> Text
tshow = pack . show

