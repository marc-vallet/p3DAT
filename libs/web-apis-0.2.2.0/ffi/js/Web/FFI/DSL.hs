
{-# LANGUAGE CPP               #-}

module Web.FFI.DSL
    ( JSVal
    , toJSBool, fromJSBool
    , toJSInt, fromJSInt
    , toJSDouble, fromJSDouble
    , toJSString, fromJSString
    , toJSText, fromJSText
    , toJSArray, fromJSArray
    , toJSCallback
    , jsNull, isNull, fromNullable
    , jsg, jsgp
    , mkDict
    , refEq
    , prop, getProp, setProp, deleteProp
    , callMeth, callMeth1, callMeth2
    , callMeth_, callMeth1_, callMeth2_, callMeth3_
    , gnew1, unsafeReadProtoTagged
    , Callback, OnBlocked(..)
    , syncCallback, syncCallback1, syncCallback2, syncCallback3
    , syncCallback', syncCallback1', syncCallback2', syncCallback3'
    , asyncCallback, asyncCallback1, asyncCallback2, asyncCallback3
    , releaseCallback
    , mkJSException
    )
where
import           Web.FFI.Types

import           Data.Text (Text, pack, unpack)
#ifdef GHCJS_NEW
import           GHC.JS.Prim hiding (getProp)
import qualified GHC.JS.Prim as JSPrim
import           GHC.JS.Foreign.Callback
#elif GHCJS_OLD
import           GHCJS.Prim hiding (getProp)
import qualified GHCJS.Prim as JSPrim
import           GHCJS.Foreign.Callback
#endif
import           System.IO.Unsafe

--------------------------------------------

jsg :: String -> IO JSVal
jsg name = ffi_jsg (toJSString name)

foreign import javascript unsafe
#if GHCJS_NEW
  "((n) => { return globalThis[n]; })"
#else
  "$r = globalThis[$1]"
#endif
  ffi_jsg :: JSVal -> IO JSVal

jsgp :: String -> JSVal
jsgp name = unsafePerformIO $ jsg name

--------------------------------------------
-- Create a dictionary

mkDict :: [(String, JSVal)] -> IO JSVal
mkDict props = do
    dict <- ffi_mkDict0
    mapM_ (\(n,v) -> setProp n v dict) props
    pure dict

foreign import javascript unsafe
#if GHCJS_NEW
  "(() => { return {}; })"
#else
  "$r = {};"
#endif
  ffi_mkDict0 :: IO JSVal

--------------------------------------------
-- Get/Set object's properties

prop :: String -> JSVal -> JSVal
prop name jobj =
  unsafePerformIO $ getProp name jobj

getProp :: String -> JSVal -> IO JSVal
getProp name jobj =
  JSPrim.getProp jobj name

setProp :: String -> JSVal -> JSVal -> IO ()
setProp name jval jobj =
  ffi_setProp (toJSString name) jval jobj

foreign import javascript unsafe
#if GHCJS_NEW
  "((n,v,o) => { o[n] = v; })"
#else
  "$3[$1] = $2"
#endif
  ffi_setProp :: JSVal -> JSVal -> JSVal -> IO ()

deleteProp :: String -> JSVal -> IO ()
deleteProp name jobj =
  ffi_deleteProp (toJSString name) jobj

foreign import javascript unsafe
#if GHCJS_NEW
  "((n,o) => { delete o[n]; })"
#else
  "delete $2[$1]"
#endif
  ffi_deleteProp :: JSVal -> JSVal -> IO ()

--------------------------------------------
-- Call object's methods

callMeth :: String -> JSVal -> IO JSVal
callMeth name jobj =
  ffi_callMeth (toJSString name) jobj

foreign import javascript
#if GHCJS_NEW
  "((n,o) => { return o[n](); })"
#else
  "$r = $2[$1]();"
#endif
  ffi_callMeth :: JSVal -> JSVal -> IO JSVal

callMeth1 :: String -> JSVal -> JSVal -> IO JSVal
callMeth1 name jarg jobj =
  ffi_callMeth1 (toJSString name) jarg jobj

foreign import javascript
#if GHCJS_NEW
  "((n,a1,o) => { return o[n](a1); })"
#else
  "$r = $3[$1]($2);"
#endif
  ffi_callMeth1 :: JSVal -> JSVal -> JSVal -> IO JSVal

callMeth2 :: String -> JSVal -> JSVal -> JSVal -> IO JSVal
callMeth2 name jarg1 jarg2 jobj =
  ffi_callMeth2 (toJSString name) jarg1 jarg2 jobj

foreign import javascript
#if GHCJS_NEW
  "((n,a1,a2,o) => { return o[n](a1,a2); })"
#else
  "$r = $4[$1]($2,$3);"
#endif
  ffi_callMeth2 :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal


callMeth_ :: String -> JSVal -> IO ()
callMeth_ name jobj =
  ffi_callMeth_ (toJSString name) jobj

foreign import javascript
#if GHCJS_NEW
  "((n,o) => { o[n](); })"
#else
  "$2[$1]()"
#endif
  ffi_callMeth_ :: JSVal -> JSVal -> IO ()

callMeth1_ :: String -> JSVal -> JSVal -> IO ()
callMeth1_ name jarg jobj =
  ffi_callMeth1_ (toJSString name) jarg jobj

foreign import javascript
#if GHCJS_NEW
  "((n,a1,o) => { o[n](a1); })"
#else
  "$3[$1]($2)"
#endif
  ffi_callMeth1_ :: JSVal -> JSVal -> JSVal -> IO ()

callMeth2_ :: String -> JSVal -> JSVal -> JSVal -> IO ()
callMeth2_ name jarg1 jarg2 jobj =
  ffi_callMeth2_ (toJSString name) jarg1 jarg2 jobj

foreign import javascript
#if GHCJS_NEW
  "((n,a1,a2,o) => { o[n](a1,a2); })"
#else
  "$4[$1]($2,$3)"
#endif
  ffi_callMeth2_ :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()

callMeth3_ :: String -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()
callMeth3_ name jarg1 jarg2 jarg3 jobj =
  ffi_callMeth3_ (toJSString name) jarg1 jarg2 jarg3 jobj

foreign import javascript
#if GHCJS_NEW
  "((n,a1,a2,a3,o) => { o[n](a1,a2,a3); })"
#else
  "$5[$1]($2,$3,$4)"
#endif
  ffi_callMeth3_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

--------------------------------------------
-- | Instantiates a new JS t'Object'.

gnew1 :: String -> JSVal -> IO JSVal
gnew1 constrn arg1 = do
  constr <- jsg constrn
  ffi_new1 constr arg1
{-# INLINABLE gnew1 #-}

foreign import javascript safe
#ifdef GHCJS_NEW
  "(($1,$2) => { return new $1($2) })"
#else
  "$r = Reflect.construct($1, $2);"
#endif
  ffi_new1 :: JSVal -> JSVal -> IO JSVal

