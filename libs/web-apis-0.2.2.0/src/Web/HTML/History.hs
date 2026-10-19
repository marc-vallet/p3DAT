
{-# LANGUAGE CPP        #-}

module Web.HTML.History
    ( History, history
    , getLength, getState
    , back, forward, go
    , replaceState, pushState
    )
where
import           Web.FFI.Types

import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T


newtype History  = History JSVal

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(() => globalThis.history)"
  history :: History

#else
history :: History
history = undefined
#endif

type State = Text

-- Properties

getLength :: History -> IO Int
getLength his =
   js_length his

getState :: History -> IO State
getState his =
  fromJSText <$> js_state his

-- Methods

back :: History -> IO ()
back his =
  js_back his

forward :: History -> IO ()
forward his =
  js_forward his

go :: Int -> History -> IO ()
go delta his =
  js_go delta his


pushState :: State -> Text -> History -> IO ()
pushState state url his =
  js_pushState (toJSText state) (mkJSStringT url) his

replaceState :: State -> Text -> History -> IO ()
replaceState state url his =
  js_replaceState (toJSText state) (mkJSStringT url) his

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((his) => { return his.length; })"
  js_length ::  History -> IO Int

foreign import javascript unsafe
  "((his) => { return his.state; })"
  js_state ::  History -> IO JSVal

foreign import javascript unsafe
  "((his) => { his.back(); })"
  js_back ::  History -> IO ()

foreign import javascript unsafe
  "((his) => { his.forward(); })"
  js_forward ::  History -> IO ()

foreign import javascript unsafe
  "((delta,his) => { his.go(); })"
  js_go ::  Int -> History -> IO ()

foreign import javascript unsafe
  "((state,url,his) => { his.pushState(state,\"\",url); })"
  js_pushState ::  JSVal -> JSString -> History -> IO ()

foreign import javascript unsafe
  "((state,url,his) => { his.replaceState(state,\"\",url); })"
  js_replaceState ::  JSVal -> JSString -> History -> IO ()

#else
js_length = undefined
js_state = undefined
js_back = undefined
js_forward = undefined
js_go = undefined
js_pushState = undefined
js_replaceState = undefined
#endif

