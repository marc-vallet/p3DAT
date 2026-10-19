{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

{-# LANGUAGE TypeAbstractions #-}

module HVDom.DOM.Prop
  ( Prop (..)
  , ElemRef (..)
  , PropValue (..)
  , propFromText, propFromBool, propFromInt, propFromDouble
  , buildProp
  , SomeProperty (..), propEqT
  )
where
import HVDom.Machine qualified as V
import HVDom.Types
import HVDom.Utils qualified as Util

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text (Text, unpack)
import Data.Typeable
import Web.DOM.Element as DOM
import Web.Event.Event as DOM
import Web.Event.EventTarget as DOM
import Web.FFI.DSL qualified as FFI
import Web.HTML.Common as HTML

data Prop msg
  = Attribute (Maybe Namespace) AttrName Text
  | forall val. Typeable val => Property (PropName val) (PropValue val)
  | Handler EventType (Event -> Maybe msg)
  | Ref (ElemRef Element -> Maybe msg)

deriving instance Functor Prop

newtype PropValue val = PropValue FFI.JSVal

unPropValue :: PropValue a -> FFI.JSVal
unPropValue (PropValue jval) = jval

propFromText :: Text -> PropValue Text
propFromText = PropValue . FFI.toJSText

propFromBool :: Bool -> PropValue Bool
propFromBool = PropValue . FFI.toJSBool

propFromInt :: Int -> PropValue Int
propFromInt = PropValue . FFI.toJSInt

propFromDouble :: Double -> PropValue Double
propFromDouble = PropValue . FFI.toJSDouble

data SomeProperty
  = forall val. Typeable val => SomeProperty (PropName val) (PropValue val)

propNameHEq :: forall a b. (Typeable a, Typeable b)
            => PropName a -> PropName b
            -> Maybe (a :~: b)
propNameHEq (PropName n1) (PropName n2) =
  case eqT @a @b of
    Just r -> if n1 == n2 then Just r else Nothing
    Nothing -> Nothing

propEqT :: SomeProperty -> SomeProperty
        -> (forall val. Maybe (PropName val, PropValue val, PropValue val) -> r)
        -> r
propEqT (SomeProperty @t1 n1 v1) (SomeProperty @t2 n2 v2) k =
  case eqT @t1 @t2 of
    Just Refl ->
      if n1 == n2 then k $ Just (n1, v1, v2)
      else k Nothing
    Nothing -> k Nothing

data ElemRef a
  = Created a
  | Removed a
  deriving (Functor)

type RemoveEventListener = IO ()

type EventMap a = M.Map EventType (RemoveEventListener, IORef (Event -> Maybe a))

data PropState a = PropState
  { events :: IORef (EventMap a)
  , props :: M.Map Text (Prop a)
  }

propToStrKey :: Prop i -> Text
propToStrKey = \case
  Attribute (Just (Namespace ns)) (AttrName attr) _ -> "attr/" <> ns <> ":" <> attr
  Attribute _ (AttrName attr) _ -> "attr/:" <> attr
  Property (PropName prop) _ -> "prop/" <> prop
  Handler (DOM.EventType ty) _ -> "handler/" <> ty
  Ref _ -> "ref"

buildProp
  :: forall a
   . (a -> IO ())
  -> DOM.Element
  -> V.Machine [Prop a] ()
buildProp emit el = renderProp
  where
    renderProp :: V.Machine [Prop a] ()
    renderProp ps1 = do
      events <- newIORef mempty
      ps1' <- Util.strMapWithIxE ps1 propToStrKey (applyProp events)
      let state =
            PropState
              { events
              , props = ps1'
              }
      pure $ V.Step () state patchProp haltProp

    patchProp :: PropState a -> [Prop a] -> IO (V.Step [Prop a] ())
    patchProp state ps2 = do
      events <- newIORef mempty
      let PropState {events = prevEvents, props = ps1} = state
          onThese = diffProp prevEvents events
          onThis = removeProp prevEvents
          onThat = applyProp events
      props <- Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
      let nextState = PropState { events, props }
      pure $ V.Step () nextState patchProp haltProp

    haltProp :: PropState a -> IO ()
    haltProp state = do
      case M.lookup "ref" state.props of
        Just (Ref f) ->
          mbEmit (f (Removed el))
        _ -> pure ()

    mbEmit = traverse_ emit

    applyProp :: IORef (EventMap a) -> Text -> Int -> Prop a -> IO (Prop a)
    applyProp events _ _ v =
      case v of
        Attribute mbns (AttrName attr) val -> do
          case unNamespace <$> mbns of
            Nothing -> setAttribute attr val el
            Just ns -> setAttributeNS ns attr val el
          pure v
        Property prop val -> do
          setProperty prop val el
          pure v
        Handler evty f -> do
          mbev <- M.lookup evty <$> readIORef events
          case mbev of
              Just (_, handler) -> do
                atomicWriteIORef handler f
                pure v
              Nothing -> do
                ref <- newIORef f
                let listener ev = do
                        f' <- readIORef ref
                        mbEmit (f' ev)
                rm <- addEventListener evty listener False $ toEventTarget el
                atomicModifyIORef' events ((,()) . M.insert evty (rm, ref))
                pure v
        Ref f -> do
          mbEmit (f (Created el))
          pure v

    value_ :: PropName Text
    value_ = PropName "value"

    diffProp
      :: IORef (EventMap a)
      -> IORef (EventMap a)
      -> Text
      -> Int
      -> Prop a
      -> Prop a
      -> IO (Prop a)
    diffProp prevEvents events _ _ v1 v2 = do
      case (v1, v2) of
        (Attribute _ _ val1, Attribute mbns2 (AttrName attr2) val2) ->
          when (val1 /= val2) $
            case unNamespace <$> mbns2 of
              Nothing -> setAttribute attr2 val2 el
              Just ns2 -> setAttributeNS ns2 attr2 val2 el
        (Property @_ @t1 _ val1, Property @_ @t2 prop2 val2) ->
          case eqT @t1 @t2 of
            Just Refl ->
              if refEq val1 val2 then
                pure ()
              else case propNameHEq prop2 value_ of
                Just Refl -> do
                  elVal <- unsafeGetProperty value_ el
                  when (not $ refEq elVal val2) $
                    setProperty prop2 val2 el
                Nothing ->
                  setProperty prop2 val2 el
            Nothing ->
              setProperty prop2 val2 el
        (Handler _ _, Handler ty f) -> do
          handler <- (M.! ty) <$> readIORef prevEvents
          atomicWriteIORef (snd handler) f
          atomicModifyIORef' events ((,()) . M.insert ty handler)
        (_, _) ->
          pure ()
      pure v2

    removeProp prevEvents _ v =
      case v of
        Attribute mbns (AttrName attr) _ ->
          case unNamespace <$> mbns of
            Nothing -> removeAttribute attr el
            Just ns -> removeAttributeNS ns attr el
        Property prop _ ->
          removeProperty prop el
        Handler evty _ -> do
          (remhandler, _) <- (M.! evty) <$> readIORef prevEvents
          remhandler
        Ref _ -> pure ()

refEq :: PropValue a -> PropValue a -> Bool
refEq (PropValue jval1) (PropValue jval2) =
  FFI.refEq jval1 jval2

unsafeGetProperty :: PropName a -> Element -> IO (PropValue a)
unsafeGetProperty (PropName name) (Element jel) =
  PropValue <$> FFI.getProp (unpack name) jel

setProperty :: PropName a -> PropValue a -> Element -> IO ()
setProperty name@(PropName name') val el@(Element jel) = do
  oldval <- unsafeGetProperty name el
  if not $ refEq oldval val then
    FFI.setProp (unpack name') (unPropValue val) jel
  else
    pure ()

removeProperty :: PropName a -> Element -> IO ()
removeProperty (PropName name) (Element jel) =
  FFI.deleteProp (unpack name) jel

