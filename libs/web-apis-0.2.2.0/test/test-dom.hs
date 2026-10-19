
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM_)
import Web.Console
import Web.DOM.Document
import Web.DOM.Element
import Web.DOM.Node
import Web.DOM.ParentNode
import Web.HTML.Window
import Web.FFI.Types (JSVal, toJSString, toJSText)

main :: IO ()
main = do
  win <- window
  doc <- document win
  Just body <- querySelector (QuerySelector "body") doc
  button <- createElement "button" doc
  span <- createElement "span" doc
  label <- createTextNode "test-dom" doc
  insertBefore label Nothing span
  insertBefore (toNode span) Nothing button
  appendChild (toNode button) body
  let test1 = do
        let aName = "text-color"
            aValue = "red"
        replicateM_ 1000000 $ do
          setAttribute aName aValue span
          removeAttribute aName span
      test2 = do
        let aName = "text-color"
            aValue = "red"
        replicateM_ 1000000 $ do
          setAttribute2 aName aValue span
          removeAttribute2 aName span
      test3 = do
        let aName = toJSString3 "text-color"
            aValue = toJSString3 "red"
        replicateM_ 1000000 $ do
          setAttribute3 aName aValue span
          removeAttribute3 aName span
  (_, elapsed1) <- monitor_time test1
  consoleLogStr $ "elapsed time 1: " ++ show elapsed1
  (_, elapsed2) <- monitor_time test2
  consoleLogStr $ "elapsed time 2: " ++ show elapsed2
  (_, elapsed3) <- monitor_time test3
  consoleLogStr $ "elapsed time 3: " ++ show elapsed3

monitor_time :: IO a -> IO (a, Int)
monitor_time io = do
  t0 <- getMillis
  x <- io
  t1 <- getMillis
  pure (x, t1 - t0)

foreign import javascript unsafe
  "(() => { return Date.now(); })"
  getMillis :: IO Int


setAttribute2 :: String -> String -> Element -> IO ()
setAttribute2 name val self =
  js_setAttribute (toJSString name) (toJSString val) self
{-# INLINE setAttribute2 #-}

removeAttribute2 :: String -> Element -> IO ()
removeAttribute2 name self =
  js_removeAttribute (toJSString name) self
{-# INLINE removeAttribute2 #-}


newtype JSString = JSString { unJSString :: JSVal }

toJSString3 :: String -> JSString
toJSString3 = JSString . toJSString
{-# INLINE toJSString3 #-}

setAttribute3 :: JSString -> JSString -> Element -> IO ()
setAttribute3 name val self =
  js_setAttribute (unJSString name) (unJSString val) self
{-# INLINE setAttribute3 #-}

removeAttribute3 :: JSString -> Element -> IO ()
removeAttribute3 name self =
  js_removeAttribute (unJSString name) self
{-# INLINE removeAttribute3 #-}


foreign import javascript safe
  "((n,v,o) => { o.setAttribute(n,v); })"
  js_setAttribute :: JSVal -> JSVal -> Element -> IO ()

foreign import javascript safe
  "((n,o) => { o.removeAttribute(n); })"
  js_removeAttribute :: JSVal -> Element -> IO ()

