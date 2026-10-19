
{-# LANGUAGE CPP        #-}

module Web.Random
    ( -- ** Types
    StdGen
    -- ** Functions
    , genWord32
    , mkStdGen, newStdGen
    -- ** Globals
    , globalStdGen, getStdGen, setStdGen
    )
where
import           Data.Int
import           Data.IORef
import           Data.Word
import qualified Web.FFI.Types as FFI
import           System.IO.Unsafe (unsafePerformIO)

-- | t'StdGen' holds a JS t'Function'.
newtype StdGen = StdGen StateGen

type StateGen = Int32

-- | Get the next t'StdGen', extracting the 'Word32' value.
genWord32 :: StdGen -> (Word32, StdGen)
genWord32 (StdGen s1) =
  let (result, s2) = splitmix32 s1
  in (result, StdGen s2)


-- | Constructs a 'StdGen' deterministically from an `Int` seed.
-- Like 'Web.Random.newStdGen' but takes a seed as an argument and is pure.
mkStdGen :: Int -> StdGen
mkStdGen seed =
  StdGen $ fromIntegral seed

-- | Create a new 'StdGen' using system entropy when it is
-- available, while falling back on using system time as the seed.
newStdGen :: IO StdGen
newStdGen = do
  seed <- cryptoValue_ffi
  pure $ StdGen seed


-- | Global 'StdGen', used by 'getStdGen' and others.
globalStdGen :: IORef StdGen
{-# NOINLINE globalStdGen #-}
globalStdGen = unsafePerformIO $ do
  seed <- floor . (*1e7) <$> mathRandom_ffi
  newIORef (mkStdGen seed)

-- | Gets the global pseudo-random number generator.
getStdGen :: IO StdGen
getStdGen = readIORef globalStdGen

-- | Set the global pseudo-random number generator.
setStdGen :: StdGen -> IO ()
setStdGen = atomicWriteIORef globalStdGen


-----------------------------------------------------------------------------
-- FFI internal functions.

#ifdef GHCJS_NEW
foreign import javascript "(() => { return Math.random(); })"
  mathRandom_ffi :: IO Double

-- | Uses the first element of 'crypto.getRandomValues()'.
--
foreign import javascript "Web_Random.cryptoValue"
  cryptoValue_ffi :: IO Int32

-- | Uses the 'splitmix' function to generate a PRNG.
--
foreign import javascript "Web_Random.splitmix32"
  splitmix32_ffi :: StateGen -> FFI.JSVal

#else
mathRandom_ffi :: IO Double
mathRandom_ffi = undefined

cryptoValue_ffi :: IO Int32
cryptoValue_ffi = undefined

splitmix32_ffi :: StateGen -> FFI.JSVal
splitmix32_ffi = undefined
#endif

splitmix32 :: StateGen -> (Word32, StateGen)
splitmix32 s1 =
  let j = splitmix32_ffi s1
  in (fromIntegral (fst_ffi j), snd_ffi j)

#ifdef GHCJS_NEW
foreign import javascript "((p) => p.fst)"
  fst_ffi :: FFI.JSVal -> Int32

foreign import javascript "((p) => p.snd)"
  snd_ffi :: FFI.JSVal -> StateGen

#else
fst_ffi :: FFI.JSVal -> Int32
fst_ffi = undefined

snd_ffi :: FFI.JSVal -> StateGen
snd_ffi = undefined
#endif

