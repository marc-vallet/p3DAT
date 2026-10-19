
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
import           Control.Exception
import           Data.String
import           Data.Text (Text, pack, unpack)

--------------------------------------------
-- FFI types

data JSVal

data Callback f


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

toJSString :: String -> JSVal
toJSString = undefined

fromJSString :: JSVal -> String
fromJSString = undefined

toJSText :: Text -> JSVal
toJSText = toJSString . unpack

fromJSText :: JSVal -> Text
fromJSText = pack . fromJSString

toJSBool :: Bool -> JSVal
toJSBool = undefined

fromJSBool :: JSVal -> Bool
fromJSBool = undefined

toJSInt :: Int -> JSVal
toJSInt = undefined

fromJSInt :: JSVal -> Int
fromJSInt = undefined

toJSDouble :: Double -> JSVal
toJSDouble = undefined

fromJSDouble :: JSVal -> Double
fromJSDouble = undefined

toJSArray :: [JSVal] -> IO JSVal
toJSArray = undefined

fromJSArray :: JSVal -> IO [JSVal]
fromJSArray = undefined

refEq :: JSVal -> JSVal -> Bool
refEq = undefined


unsafeReadProtoTagged :: String -> JSVal -> Maybe JSVal
unsafeReadProtoTagged name value = undefined

--------------------------------------------
-- Nullable

jsNull :: JSVal
jsNull = undefined

isNull :: JSVal -> Bool
isNull = undefined

fromNullable :: (JSVal -> a) -> JSVal -> Maybe a
fromNullable con jval =
  if isNull jval then Nothing
  else Just (con jval)

--------------------------------------------
-- | Callbacks.

data OnBlocked = ThrowWouldBlock | ContinueAsync

toJSCallback :: Callback f -> JSVal
toJSCallback = undefined


syncCallback :: OnBlocked -> IO () -> IO (Callback (IO ()))
syncCallback = undefined

syncCallback1 :: OnBlocked -> (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
syncCallback1 = undefined

syncCallback2 :: OnBlocked -> (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
syncCallback2 = undefined

syncCallback3 :: OnBlocked -> (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
syncCallback3 = undefined

syncCallback' :: IO JSVal -> IO (Callback (IO JSVal))
syncCallback' = undefined

syncCallback1' :: (JSVal -> IO JSVal) -> IO (Callback (JSVal -> IO JSVal))
syncCallback1' = undefined

syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback2' = undefined

syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallback3' = undefined

asyncCallback :: IO () -> IO (Callback (IO ()))
asyncCallback = undefined

asyncCallback1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
asyncCallback1 = undefined

asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
asyncCallback2 = undefined

asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
asyncCallback3 = undefined

releaseCallback :: Callback f -> IO ()
releaseCallback = undefined

--------------------------------------------
-- | Exceptions.

type JSException = IOException

mkJSException :: JSVal -> IO JSException
mkJSException = undefined

