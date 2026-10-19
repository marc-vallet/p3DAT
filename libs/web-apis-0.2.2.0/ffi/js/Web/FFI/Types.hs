
{-# LANGUAGE CPP               #-}

module Web.FFI.Types
    ( JSVal
    , toJSBool, fromJSBool
    , toJSInt, fromJSInt
    , toJSDouble, fromJSDouble
    , toJSString, fromJSString
    , toJSText, fromJSText
    , toJSArray, fromJSArray
    , JSString, mkJSString, unJSString, mkJSStringT, unJSStringT
    , toJSCallback
    , Callback, OnBlocked(..)
    , jsNull, isNull, fromNullable
    , refEq
    , unsafeReadProtoTagged
    , syncCallback, syncCallback1, syncCallback2, syncCallback3
    , syncCallback', syncCallback1', syncCallback2', syncCallback3'
    , asyncCallback, asyncCallback1, asyncCallback2, asyncCallback3
    , releaseCallback
    , mkJSException
    )
where
import           Data.String
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

--------------------------------------------
-- FFI types

newtype JSString = JSString JSVal

instance IsString JSString where
  fromString = mkJSString

mkJSString :: String -> JSString
mkJSString = JSString . toJSString
{-# INLINABLE mkJSString #-}

unJSString :: JSString -> String
unJSString (JSString jval) = fromJSString jval
{-# INLINABLE unJSString #-}

mkJSStringT :: Text -> JSString
mkJSStringT = JSString . toJSText
{-# INLINABLE mkJSStringT #-}

unJSStringT :: JSString -> Text
unJSStringT (JSString jval) = fromJSText jval
{-# INLINABLE unJSStringT #-}

--------------------------------------------
-- Conversion

toJSText :: Text -> JSVal
toJSText = toJSString . unpack

fromJSText :: JSVal -> Text
fromJSText = pack . fromJSString

foreign import javascript unsafe
#if GHCJS_NEW
  "((x) => { return x; })"
#else
  "$r = $1"
#endif
  toJSBool :: Bool -> JSVal

foreign import javascript unsafe
#if GHCJS_NEW
  "((x) => { return x; })"
#else
  "$r = $1"
#endif
  fromJSBool :: JSVal -> Bool

foreign import javascript unsafe
#if GHCJS_NEW
  "((x) => { return x; })"
#else
  "$r = $1"
#endif
  toJSDouble :: Double -> JSVal

foreign import javascript unsafe
#if GHCJS_NEW
  "((x) => { return x; })"
#else
  "$r = $1"
#endif
  fromJSDouble :: JSVal -> Double

foreign import javascript unsafe
#if GHCJS_NEW
  "((x) => { return x; })"
#else
  "$r = $1"
#endif
  toJSCallback :: Callback f -> JSVal

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((o1,o2) => { return o1 === o2; })"
#else
  "$r = $1 === $2;"
#endif
  refEq :: JSVal -> JSVal -> Bool


unsafeReadProtoTagged :: String -> JSVal -> Maybe JSVal
unsafeReadProtoTagged name jobj =
  fromNullable id $
    ffi_unsafeReadProtoTagged (toJSString name) jobj

foreign import javascript safe
#ifdef GHCJS_NEW
  "_unsafeReadProtoTagged"
#else
  "$r = _unsafeReadProtoTagged($1, $2);"
#endif
  ffi_unsafeReadProtoTagged :: JSVal -> JSVal -> JSVal

--------------------------------------------
-- Nullable

fromNullable :: (JSVal -> a) -> JSVal -> Maybe a
fromNullable con jval =
  if isNull jval then Nothing
  else Just (con jval)

