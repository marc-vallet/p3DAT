{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module HVDom.Thunk
where
import HVDom qualified as V

import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName
import Web.DOM.Node

data ThunkId =
  forall a. ThunkId (StableName a)

unsafeThunkId :: a -> ThunkId
unsafeThunkId x = unsafePerformIO $ do
    sname <- x `seq` makeStableName x
    pure $ ThunkId sname

unsafeEqThunkId :: ThunkId -> ThunkId -> Bool
unsafeEqThunkId (ThunkId n1) (ThunkId n2) =
  eqStableName n1 n2

data Thunk f i =
  forall a. Typeable a => Thunk ThunkId (a -> a -> Bool) (a -> f i) a

deriving instance (Functor f) => Functor (Thunk f)

unsafeEqThunk :: forall f i. Thunk f i -> Thunk f i -> Bool
unsafeEqThunk (Thunk n1 eq1 _ x1) (Thunk n2 _ _ x2) =
  case cast x2 of
    Just x2' -> unsafeEqThunkId n1 n2 && eq1 x1 x2'
    Nothing -> False

data ThunkState f i a w = ThunkState
  { vdom :: V.Step (V.VDom a w) Node
  , thunk :: Thunk f i
  }

hoist :: forall f g a. (forall x. f x -> g x) -> Thunk f a -> Thunk g a
hoist f = mapThunk f

mapThunk :: forall f g i j. (f i -> g j) -> Thunk f i -> Thunk g j
mapThunk k (Thunk a b c d) = Thunk a b (k . c) d

runThunk :: forall f i. Thunk f i -> f i
runThunk (Thunk _ _ render arg) = render arg

buildThunk
  :: forall f i a w
   . (f i -> V.VDom a w)
  -> V.VDomSpec a w
  -> V.Machine (Thunk f i) Node
buildThunk toVDom = renderThunk
  where
    renderThunk :: V.VDomSpec a w -> V.Machine (Thunk f i) Node
    renderThunk spec t = do
      vdom <- V.buildVDom spec (toVDom (runThunk t))
      pure $ V.Step (V.extract vdom) (ThunkState {thunk = t, vdom}) patchThunk haltThunk

    patchThunk :: ThunkState f i a w -> Thunk f i -> IO (V.Step (Thunk f i) Node)
    patchThunk state t2 = do
      let ThunkState {vdom = prev, thunk = t1} = state
      if unsafeEqThunk t1 t2
        then pure $ V.Step (V.extract prev) state patchThunk haltThunk
        else do
          vdom <- V.step prev (toVDom (runThunk t2))
          pure $ V.Step (V.extract vdom) (ThunkState {vdom, thunk = t2}) patchThunk haltThunk

    haltThunk :: ThunkState f i a w -> IO ()
    haltThunk state = V.halt state.vdom

