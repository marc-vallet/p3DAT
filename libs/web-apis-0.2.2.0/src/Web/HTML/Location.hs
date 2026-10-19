
{-# LANGUAGE CPP        #-}

module Web.HTML.Location
    ( Location, location
    , getHref, setHref
    , getPathname, getSearch, getHash, getOrigin
    , replace, reload
    )
where
import           Web.FFI.Types

import           Data.Text (Text, pack, unpack)


newtype Location  = Location JSVal

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(() => globalThis.location)"
  location :: Location

#else
location :: Location
location = undefined
#endif

getHref :: Location -> IO Text
getHref loc = unJSStringT <$> js_href loc

setHref :: Text -> Location -> IO ()
setHref url loc = js_set_href (mkJSStringT url) loc

getPathname :: Location -> IO Text
getPathname loc = unJSStringT <$> js_pathname loc

getSearch :: Location -> IO Text
getSearch loc = unJSStringT <$> js_search loc

getHash :: Location -> IO Text
getHash loc = unJSStringT <$> js_hash loc

getOrigin :: Location -> IO Text
getOrigin loc = unJSStringT <$> js_origin loc

replace :: Text -> Location -> IO ()
replace url loc = js_replace (mkJSStringT url) loc

#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((loc) => { loc.reload(); })"
  reload :: Location -> IO ()

#else
reload :: Location -> IO ()
reload = undefined
#endif


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((loc) => { return loc.href; })"
  js_href :: Location -> IO JSString

foreign import javascript unsafe
  "((txt,loc) => { loc.href = txt; })"
  js_set_href :: JSString -> Location -> IO ()

foreign import javascript unsafe
  "((loc) => { return loc.pathname; })"
  js_pathname :: Location -> IO JSString

foreign import javascript unsafe
  "((loc) => { return loc.search; })"
  js_search :: Location -> IO JSString

foreign import javascript unsafe
  "((loc) => { return loc.hash; })"
  js_hash :: Location -> IO JSString

foreign import javascript unsafe
  "((loc) => { return loc.origin; })"
  js_origin :: Location -> IO JSString

foreign import javascript unsafe
  "((txt,loc) => { loc.replace(txt); })"
  js_replace :: JSString -> Location -> IO ()

#else
js_href = undefined
js_set_href = undefined
js_pathname = undefined
js_search = undefined
js_hash = undefined
js_origin = undefined
js_replace = undefined
#endif

