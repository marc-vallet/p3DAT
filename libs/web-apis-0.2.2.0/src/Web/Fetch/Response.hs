
{-# LANGUAGE CPP               #-}

module Web.Fetch.Response
    ( Response(..)
    , text, json
    )
where
import           Control.Monad (forM)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as J
import qualified Data.Aeson.KeyMap as J
import           Data.Text (Text, pack)
import qualified Data.Vector as V
import qualified Web.FFI.Types as FFI
import           Web.IDL.Promise

newtype Response = Response FFI.JSVal

text :: Response -> IO Text
text resp =
  (pack . FFI.fromJSString) <$> (text_ffi resp >>= await') 

json :: Response -> IO J.Value
json resp =
  json_ffi resp >>= await' >>= fromJSValue


#ifdef GHCJS_NEW
foreign import javascript "((resp) => { return resp.text(); })"
  text_ffi :: Response -> IO Promise'

foreign import javascript "((resp) => { return resp.json(); })"
  json_ffi :: Response -> IO Promise'
#else

text_ffi :: Response -> IO Promise'
text_ffi = undefined

json_ffi :: Response -> IO Promise'
json_ffi = undefined
#endif

-----------------------------------------------------------------------------

data ValueType = NullTy | NumberTy | StringTy | BoolTy | ArrayTy | ObjectTy
              -- 0        1          2          3        4         5
    deriving (Eq, Enum)

fromJSValue :: FFI.JSVal -> IO J.Value
fromJSValue jsval_ = do
  t <- typeof jsval_
  case t of
    0 -> pure J.Null
    1 -> pure $ J.toJSON $ FFI.fromJSDouble jsval_
    2 -> pure $ J.String $ pack $ FFI.fromJSString jsval_
    3 -> pure $ J.Bool $ FFI.fromJSBool jsval_
    4 -> do jxs <- FFI.fromJSArray jsval_
            xs <- forM jxs fromJSValue
            pure $ J.Array $ V.fromList xs
    _ -> do keys <- listJSProps jsval_
            result <- forM keys $ \key -> do
                raw <- getJSProp key jsval_
                value <- fromJSValue raw
                pure (key, value)
            pure $ J.Object $ J.fromList result

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return Web_Fetch.typeOf($1); })"
  typeof :: FFI.JSVal -> IO Int
#else

typeof :: FFI.JSVal -> IO Int
typeof = undefined
#endif

listJSProps :: FFI.JSVal -> IO [J.Key]
listJSProps jsval = do
  jks <- listJSProps_ffi jsval
  jks' <- FFI.fromJSArray jks
  pure $ (J.fromString . FFI.fromJSString) <$> jks'

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return Object.keys($1); })"
  listJSProps_ffi :: FFI.JSVal -> IO FFI.JSVal
#else

listJSProps_ffi :: FFI.JSVal -> IO FFI.JSVal
listJSProps_ffi = undefined
#endif

getJSProp :: J.Key -> FFI.JSVal -> IO FFI.JSVal
getJSProp key =
  getJSProp_ffi (FFI.toJSString $ J.toString key)

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((k,o) => { return o[k]; })"
  getJSProp_ffi :: FFI.JSVal -> FFI.JSVal -> IO FFI.JSVal
#else

getJSProp_ffi :: FFI.JSVal -> FFI.JSVal -> IO FFI.JSVal
getJSProp_ffi = undefined
#endif

