
{-# LANGUAGE CPP               #-}

module Web.Fetch.Request
    ( RequestInit, emptyRI
    , Request, new
    )
where
import           Data.Text (Text, unpack)
import qualified Web.FFI.Types as FFI

newtype RequestInit = RequestInit FFI.JSVal

#ifdef GHCJS_NEW
foreign import javascript "(() => { return {}; })"
  emptyRI :: RequestInit
#else
emptyRI :: RequestInit
emptyRI = undefined
#endif

newtype Request = Request FFI.JSVal

new :: Text -> RequestInit -> IO Request
new url options =
  new_ffi (FFI.toJSText url) options

#ifdef GHCJS_NEW
foreign import javascript "((url, opts) => { return Web_Fetch.newRequest(url, opts); })"
  new_ffi :: FFI.JSVal -> RequestInit -> IO Request

#else
new_ffi :: FFI.JSVal -> RequestInit -> IO Request
new_ffi = undefined
#endif

