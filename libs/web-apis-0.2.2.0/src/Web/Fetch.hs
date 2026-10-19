
{-# LANGUAGE CPP               #-}

module Web.Fetch
    ( fetch
    , module Web.Fetch.Request
    , module Web.Fetch.Response
    )
where
import           Data.Text (Text)
import           Web.Fetch.Request
import           Web.Fetch.Response
import           Web.IDL.Promise

fetch
  :: Text
  -- ^ url
  -> RequestInit
  -- ^ options
  -> IO Response
fetch url options = do
  req <- new url options
  p <- fetchCore req
  await p

fetchCore :: Request -> IO (Promise Response)
fetchCore req =
  Promise (pure . Response) <$> fetchCore_ffi req

#ifdef GHCJS_NEW
foreign import javascript safe "((req) => { return fetch(req, {}); })"
  fetchCore_ffi :: Request -> IO Promise'

#else
fetchCore_ffi :: Request -> IO Promise'
fetchCore_ffi = undefined
#endif

