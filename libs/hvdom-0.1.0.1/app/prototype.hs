
{-# LANGUAGE CPP #-}

-- #define V1
#define V2

module Main
where
import Prelude hiding (elem)
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
#ifdef V1
import Control.Monad (when, void)
#endif
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.IORef
#ifdef V1
import Data.Maybe (isNothing)
#endif
import Data.Text (Text, pack)
import Data.Typeable
import Data.Void
import HVDom qualified as V
import HVDom.DOM.Prop (Prop(..), propFromText, buildProp)
import HVDom.Thunk (Thunk(..), unsafeThunkId, buildThunk)
import Web.Console (consoleLog)
import Web.DOM.Document qualified as DOM (Document)
import Web.DOM.Node qualified as DOM (Node, toNode, appendChild)
import Web.DOM.ParentNode qualified as DOM (QuerySelector(..), querySelector)
import Web.FFI.DSL qualified as FFI
import Web.HTML.Common qualified as DOM (PropName(..))
import Web.HTML.HTMLDocument qualified as DOM (toDocument, toParentNode)
import Web.HTML.Window qualified as DOM (window, document)

infixr 1 <:=

(<:=) :: forall a. DOM.PropName Text -> Text -> Prop a
key <:= val = Property key (propFromText val)

newtype VDom a = VDom (V.VDom [Prop a] (Thunk VDom a))

unVDom :: VDom a -> V.VDom [Prop a] (Thunk VDom a)
unVDom (VDom vdom) = vdom

instance Functor VDom where
  fmap f (VDom vdom) = VDom (bimap (map (fmap f)) (fmap f) vdom)

type State = [Database]

data Database = Database
  { dbname :: Text
  , lastSample :: LastSample
  }
  deriving Eq

data LastSample = LastSample
  { countClassName :: Text
  , nbQueries :: Int
  , topFiveQueries :: [DBQuery]
  }
  deriving Eq

data DBQuery = DBQuery
  { elapsedClassName :: Text
  , formatElapsed :: Text
  , query :: Text
  }
  deriving Eq

fromJSDatabase :: FFI.JSVal -> IO Database
fromJSDatabase jref = do
  consoleLog "fromJSDatabase"
  Database <$> (FFI.fromJSText <$> FFI.getProp "dbname" jref)
           <*> (FFI.getProp "lastSample" jref >>= fromJSLastSample)

fromJSLastSample :: FFI.JSVal -> IO LastSample
fromJSLastSample jref = do
  consoleLog "fromJSLastSample"
  LastSample <$> (FFI.fromJSText <$> FFI.getProp "countClassName" jref)
             <*> (FFI.fromJSInt <$> FFI.getProp "nbQueries" jref)
             <*> (FFI.getProp "topFiveQueries" jref >>= FFI.fromJSArray >>= mapM fromJSDBQuery)

fromJSDBQuery :: FFI.JSVal -> IO DBQuery
fromJSDBQuery jref = do
  consoleLog "fromJSDBQuery"
  DBQuery <$> (FFI.fromJSText <$> FFI.getProp "elapsedClassName" jref)
          <*> (FFI.fromJSText <$> FFI.getProp "formatElapsed" jref)
          <*> (FFI.fromJSText <$> FFI.getProp "query" jref)

initialState :: State
initialState = []

elem :: forall a. Text -> [Prop a] -> [VDom a] -> VDom a
elem n a c = VDom $ V.Elem Nothing (V.ElemName n) a (unVDom <$> c)

keyed :: forall a. Text -> [Prop a] -> [(Text, VDom a)] -> VDom a
keyed n a c = VDom $ V.Keyed Nothing (V.ElemName n) a (second unVDom <$> c)

text :: forall a. Text -> VDom a
text a = VDom $ V.Text a

thunk :: forall a b. (Typeable a, Eq a) => (a -> VDom b) -> a -> VDom b
thunk render val = VDom $ V.Widget $ thunk1 render val

thunk1 :: forall a f i. (Typeable a, Eq a) => (a -> f i) -> a -> Thunk f i
thunk1 f a = Thunk (unsafeThunkId f) (==) f a

className_ :: DOM.PropName Text
className_ = DOM.PropName "className"

renderData :: State -> VDom Void
renderData st =
  elem "div" []
    [ elem "table"
        [ className_ <:= "table table-striped latest data" ]
        [ keyed "tbody" [] (map (\db -> (db.dbname, thunk renderDatabase db)) st) ]
        -- [ keyed "tbody" [] (map (\db -> (db.dbname, renderDatabase db)) st) ]
        -- [ elem "tbody" [] (map (thunk renderDatabase) st) ]
        -- [ elem "tbody" [] (map renderDatabase st) ]
    ]

  where
  renderDatabase db =
    elem "tr"
      []
      ([ elem "td"
          [ className_ <:= "dbname" ]
          [ text db.dbname ]
      , elem "td"
          [ className_ <:= "query-count" ]
          [ elem "span"
              [ className_ <:= db.lastSample.countClassName ]
              [ text (pack $ show db.lastSample.nbQueries) ]
          ]
      ] <> map renderQuery db.lastSample.topFiveQueries)

  renderQuery q =
    elem "td"
      [ className_ <:= "Query" <> q.elapsedClassName ]
      [ text q.formatElapsed
      , elem "div"
          [ className_ <:= "popover left" ]
          [ elem "div"
              [ className_ <:= "popover-content" ]
              [ text q.query ]
          , elem "div"
              [ className_ <:= "arrow" ]
              []
          ]
      ]

mkSpec
  :: DOM.Document
  -> V.VDomSpec [Prop Void] (Thunk VDom Void)
mkSpec document = V.VDomSpec
  { buildWidget = buildThunk unVDom
  , buildAttributes = buildProp (const $ pure ())
  , document
  }

getData :: IO State
getData = do
  consoleLog "getData"
  js_getData >>= FFI.fromJSArray >>= mapM fromJSDatabase

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "getData"
  js_getData :: IO FFI.JSVal

foreign import javascript unsafe
  "getTimeout"
  getTimeout :: IO Int

foreign import javascript unsafe
  "pingRenderRate"
  pingRenderRate :: IO ()

#else
js_getData :: IO FFI.JSVal
js_getData = undefined
getTimeout :: IO Int
getTimeout = undefined
pingRenderRate :: IO ()
pingRenderRate = undefined
#endif

{-- TODO
foreign import javascript unsafe
  "requestAnimationFrame"
  requestAnimationFrame :: IO () -> IO ()
--}

mkRenderQueue
  :: forall a
  . V.VDomSpec [Prop Void] (Thunk VDom Void)
  -> DOM.Node
  -> (a -> VDom Void)
  -> a
  -> IO (a -> IO ())
#ifdef V1
mkRenderQueue spec parent render initialValue = do
  initMachine <- V.buildVDom spec (unVDom (render initialValue))
  _ <- DOM.appendChild (V.extract initMachine) parent
  ref <- newIORef initMachine
  val <- newIORef Nothing
  pure $ \a -> do
    v <- readIORef val
    writeIORef val (Just a)
    when (isNothing v) $ requestAnimationFrame $ do
      machine <- readIORef ref
      mbv' <- readIORef val
      for_ mbv' $ \v' -> do
        res <- V.step machine (unVDom (render v'))
        writeIORef ref res
        writeIORef val Nothing
#endif

#ifdef V2
mkRenderQueue spec parent render initialValue = do
  consoleLog "mkRenderQueue"
  initMachine <- V.buildVDom spec (unVDom (render initialValue))
  _ <- DOM.appendChild (V.extract initMachine) parent
  ref <- newIORef initMachine
  pure $ \v -> do
    consoleLog "pushQueue"
    machine <- readIORef ref
    res <- V.step machine (unVDom (render v))
    writeIORef ref res
#endif

main :: IO ()
main = do
  consoleLog "main: start"
  win <- DOM.window
  doc <- DOM.document win
  bod <- DOM.querySelector (DOM.QuerySelector "body") (DOM.toParentNode doc)
  for_ bod $ \body -> do
    consoleLog "main: body"
    let spec = mkSpec (DOM.toDocument doc)
    pushQueue <- mkRenderQueue spec (DOM.toNode body) renderData initialState
    let
      loop = do
        newData <- getData
        timeout <- getTimeout
        pushQueue newData
        pingRenderRate
        threadDelay (1000 * timeout)
        loop
    loop
  consoleLog "main: end"

