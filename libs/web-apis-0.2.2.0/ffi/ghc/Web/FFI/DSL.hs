
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
import           System.IO.Unsafe

--------------------------------------------

jsg :: String -> IO JSVal
jsg name = undefined

jsgp :: String -> JSVal
jsgp name = unsafePerformIO $ jsg name

--------------------------------------------
-- Create a dictionary

mkDict :: [(String, JSVal)] -> IO JSVal
mkDict props = undefined

--------------------------------------------
-- Get/Set object's properties

prop :: String -> JSVal -> JSVal
prop name jobj =
  unsafePerformIO $ getProp name jobj

getProp :: String -> JSVal -> IO JSVal
getProp = undefined

setProp :: String -> JSVal -> JSVal -> IO ()
setProp = undefined

deleteProp :: String -> JSVal -> IO ()
deleteProp = undefined

--------------------------------------------
-- Call object's methods

callMeth :: String -> JSVal -> IO JSVal
callMeth name = undefined

callMeth1 :: String -> JSVal -> JSVal -> IO JSVal
callMeth1 name = undefined

callMeth2 :: String -> JSVal -> JSVal -> JSVal -> IO JSVal
callMeth2 name = undefined


callMeth_ :: String -> JSVal -> IO ()
callMeth_ name = undefined

callMeth1_ :: String -> JSVal -> JSVal -> IO ()
callMeth1_ name = undefined

callMeth2_ :: String -> JSVal -> JSVal -> JSVal -> IO ()
callMeth2_ name = undefined

callMeth3_ :: String -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()
callMeth3_ name = undefined

--------------------------------------------
-- | Instantiates a new JS t'Object'.

gnew1 :: String -> JSVal -> IO JSVal
gnew1 constrn arg1 = undefined

