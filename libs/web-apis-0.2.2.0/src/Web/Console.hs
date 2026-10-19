
{-# LANGUAGE CPP               #-}

module Web.Console
    ( consoleLog, consoleWarn, consoleError
    , consoleLogStr, consoleWarnStr, consoleErrorStr
    )
where
import           Data.Text (Text, unpack)
import qualified Web.FFI.Types as FFI

-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
--
-- Console logging of JavaScript strings.
consoleLog :: Text -> IO ()
{-# INLINABLE consoleLog #-}
consoleLog = consoleLogStr . unpack

-- | Outputs a warning message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/warn>
--
-- Console logging of JavaScript strings.
consoleWarn :: Text -> IO ()
{-# INLINABLE consoleWarn #-}
consoleWarn = consoleWarnStr . unpack

-- | Outputs an error message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/error>
--
-- Console logging of JavaScript strings.
consoleError :: Text -> IO ()
{-# INLINABLE consoleError #-}
consoleError = consoleErrorStr . unpack


-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
--
-- Console logging of JavaScript strings.
consoleLogStr :: String -> IO ()
{-# INLINABLE consoleLogStr #-}
consoleLogStr msg =
  js_consoleLog (FFI.mkJSString msg)

-- | Outputs a warning message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/warn>
--
-- Console logging of JavaScript strings.
consoleWarnStr :: String -> IO ()
{-# INLINABLE consoleWarnStr #-}
consoleWarnStr msg =
  js_consoleWarn (FFI.mkJSString msg)

-- | Outputs an error message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/error>
--
-- Console logging of JavaScript strings.
consoleErrorStr :: String -> IO ()
{-# INLINABLE consoleErrorStr #-}
consoleErrorStr msg =
  js_consoleError (FFI.mkJSString msg)


#ifdef GHCJS_NEW
foreign import javascript unsafe
  "((msg) => { console.log(msg); })"
  js_consoleLog :: FFI.JSString -> IO ()

foreign import javascript unsafe
  "((msg) => { console.warn(msg); })"
  js_consoleWarn :: FFI.JSString -> IO ()

foreign import javascript unsafe
  "((msg) => { console.error(msg); })"
  js_consoleError :: FFI.JSString -> IO ()

#else
js_consoleLog = undefined
js_consoleWarn = undefined
js_consoleError = undefined
#endif

