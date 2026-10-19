
{-# LANGUAGE CPP                        #-}

module Fluid.Random
    ( module System.Random
#ifdef GHCJS_BOTH
    , StdGen, mkStdGen, initStdGen
    , getStdRandom, getStdGen
    , randomIO, randomRIO
#endif
    )
where

#ifdef GHCJS_BOTH
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (atomicModifyIORef')
import           System.Random hiding (StdGen, mkStdGen, initStdGen,
                                       getStdRandom, getStdGen, setStdGen, newStdGen,
                                       randomIO, randomRIO)
import qualified Web.Random as WRand

type StdGen = WRand.StdGen

instance RandomGen WRand.StdGen where
    -- genWord32 :: g -> (Word32, g)
    genWord32 g = WRand.genWord32 g
    -- split :: g -> (g, g)
    split = error "Miso's random generator not splittable"

-- | Constructs a 'StdGen' deterministically from an `Int` seed. See `mkStdGen64` for a `Word64`
-- variant that is architecture agnostic.
mkStdGen :: Int -> StdGen
mkStdGen = WRand.mkStdGen

-- | Initialize 'StdGen' using system entropy when it is
-- available, while falling back on using system time as the seed.
initStdGen :: MonadIO m => m StdGen
initStdGen = liftIO WRand.newStdGen


-- | Gets the global pseudo-random number generator.
getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO WRand.getStdGen


-- | Uses the supplied function to get a value from the current global
-- random generator, and updates the global generator with the new generator
-- returned by the function. For example, @rollDice@ produces a pseudo-random integer
-- between 1 and 6:
--
-- >>> rollDice = getStdRandom (randomR (1, 6))
-- >>> replicateM 10 (rollDice :: IO Int)
-- [1,1,1,4,5,6,1,2,2,5]
--
-- This is an outdated function and it is recommended to switch to its
-- equivalent 'System.Random.Stateful.applyAtomicGen' instead, possibly with the
-- 'System.Random.Stateful.globalStdGen' if relying on the global state is
-- acceptable.
--
-- >>> import System.Random.Stateful
-- >>> rollDice = applyAtomicGen (uniformR (1, 6)) globalStdGen
-- >>> replicateM 10 (rollDice :: IO Int)
-- [2,1,1,5,4,3,6,6,3,2]
getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
getStdRandom f = liftIO $ atomicModifyIORef' WRand.globalStdGen (swap . f)
  where swap (v, g) = (g, v)


-- | A variant of 'System.Random.Stateful.randomRM' that uses the global
-- pseudo-random number generator 'System.Random.Stateful.globalStdGen'
--
-- >>> randomRIO (2020, 2100) :: IO Int
-- 2028
--
-- Similar to 'randomIO', this function is equivalent to @'getStdRandom'
-- 'randomR'@ and is included in this interface for historical reasons and
-- backwards compatibility. It is recommended to use
-- 'System.Random.Stateful.uniformRM' instead, possibly with the
-- 'System.Random.Stateful.globalStdGen' if relying on the global state is
-- acceptable.
--
-- >>> import System.Random.Stateful
-- >>> uniformRM (2020, 2100) globalStdGen :: IO Int
-- 2044
randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
randomRIO range = getStdRandom (randomR range)

-- | A variant of 'System.Random.Stateful.randomM' that uses the global
-- pseudo-random number generator 'System.Random.Stateful.globalStdGen'.
--
-- >>> import Data.Int
-- >>> randomIO :: IO Int32
-- 114794456
--
-- This function is equivalent to @'getStdRandom' 'random'@ and is included in
-- this interface for historical reasons and backwards compatibility. It is
-- recommended to use 'System.Random.Stateful.uniformM' instead, possibly with
-- the 'System.Random.Stateful.globalStdGen' if relying on the global state is
-- acceptable.
--
-- >>> import System.Random.Stateful
-- >>> uniformM globalStdGen :: IO Int32
-- -1768545016
randomIO :: (Random a, MonadIO m) => m a
randomIO = getStdRandom random

#else
import           System.Random

#endif

