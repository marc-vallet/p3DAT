
{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified GHC.JS.Prim as Prim
import Web.IDL.Promise 

main :: IO ()
{- -
main = do
  flip mapM_ [1..100] $ \i ->
    Conc.forkIO $
      consoleLog $
        "Aquest és un missatge de prova: i=" <> show i
  consoleLog "End of main"
- -
main = do
  x1 <- fun_test1 5
  x2 <- fun_test2 x1
  consoleLog $ "End of main. x=" <> show x2
- -
main = do
  mvar <- Conc.newMVar ()
  Conc.takeMVar mvar
  consoleLog $ "End of main"
- -}
main = do
  consoleLog "Start of main"
  tp <- newTimer 5000
  consoleLog "Waiting timer"
  await tp
  consoleLog "End of main"
{- -}

------------------

foreign import javascript unsafe
  "((i) => { console.log('fun_test1', i); return i + 1; })"
  fun_test1 :: Int -> IO Int

foreign import javascript safe
  "((i) => { console.log('fun_test2', i); return i + 1; })"
  fun_test2 :: Int -> IO Int

------------------

newTimer :: Int -> IO (Promise ())
newTimer millis = Promise (const $ pure ()) <$> newTimer_ffi millis

foreign import javascript safe "newTimer"
  newTimer_ffi :: Int -> IO Promise'

------------------

consoleLog :: String -> IO ()
consoleLog = consoleLog_ffi . Prim.toJSString

foreign import javascript unsafe
  "((s) => { console.log(s); })"
  consoleLog_ffi :: Prim.JSVal -> IO ()

