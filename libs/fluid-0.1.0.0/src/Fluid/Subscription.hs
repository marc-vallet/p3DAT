
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE TypeOperators      #-}

module Fluid.Subscription where

import Fluid.Utils.Conc
import Fluid.Utils.NT
import Fluid.Utils.UUID

import Control.Applicative
import Control.Monad (when)
import Data.Foldable
import Data.Functor.Contravariant

-- | A paired `Listener` and `Emitter` produced with the `create` function.
data Subscribe a = Subscribe
  { listener :: Listener a
  , emitter :: Emitter a
  }

create :: IO (Subscribe a)
create = do
  subscribers <- newIORef []
  pure
    $ Subscribe
      { emitter = Emitter $ \handler -> do
          uuid <- generateV4
          atomicModifyIORef'_ subscribers (<> [(handler, uuid)])
          pure $ Subscription
              { unsubscribe = atomicModifyIORef'_ subscribers (Prelude.filter ((uuid /=) . snd))
              }
      , listener = Listener subscribers
      }

newtype Emitter a = Emitter {subscribe :: (a -> IO ()) -> IO Subscription}
  deriving (Functor)

-- subscribe :: Emitter a -> (a -> IO ()) -> IO Subscription

instance Applicative Emitter where
  pure a = Emitter $ \k -> do
    k a
    pure (Subscription (pure ()))

  -- (<*>) :: Emitter (a -> b) -> Emitter a -> Emitter b
  Emitter e1 <*> Emitter e2 = Emitter $ \k -> do
    -- e1 :: ((a -> b) -> IO ()) -> IO Subscription
    -- e2 :: (a -> IO ()) -> IO Subscription
    -- k :: b -> IO ()
    latest1 <- newIORef Nothing
    latest2 <- newIORef Nothing
    -- latest1 :: IORef (Maybe (a -> b))
    -- latest2 :: IORef (Maybe a)
    Subscription c1 <- e1 $ \x1 -> do
      -- x1 :: a -> b
      atomicWriteIORef latest1 (Just x1)
      readIORef latest2 >>= traverse_ (k . x1)
    Subscription c2 <- e2 $ \x2 -> do
      -- x2 :: a
      atomicWriteIORef latest2 (Just x2)
      readIORef latest1 >>= traverse_ (k . ($ x2))
    pure (Subscription (c1 *> c2))

instance Alternative Emitter where
  empty = Emitter $ \_ -> pure (Subscription (pure ()))
  Emitter f <|> Emitter g = Emitter $ \k -> do
    Subscription c1 <- f k
    Subscription c2 <- g k
    pure (Subscription (c1 *> c2))

makeEmitter
  :: ((a -> IO ()) -> IO (IO ()))
  -> Emitter a
makeEmitter f = Emitter (fmap Subscription . f)

----------------------------------------------------------------------

newtype Listener a = Listener (IORef [(a -> IO (), UUID)])
    deriving Eq

notify :: Listener a -> a -> IO ()
notify (Listener subscribersRef) a = do
  subs <- readIORef subscribersRef
  traverse_ (($ a) . fst) subs

--- TODO:
---instance Contravariant Listener where
---  contramap f (Listener g) = Listener (g . f)

newtype Subscription = Subscription {unsubscribe :: IO ()}

-- unsubscribe :: Subscription -> IO ()

fold :: (a -> b -> b) -> Emitter a -> b -> Emitter b
fold f (Emitter e) b = Emitter $ \k -> do
  result <- newIORef b
  e $ \a ->
    let g b1 = let b2 = f a b1 in (b2, b2)
    in atomicModifyIORef' result g >>= k

filter :: (a -> Bool) -> Emitter a -> Emitter a
filter p (Emitter e) = Emitter $ \k -> e $ \a -> when (p a) (k a)

{-
-- | Compute a fixed point.
fix :: forall i o. (Emitter i -> (Emitter i, Emitter o)) -> Emitter o
fix f = Emitter $ \k -> do
  Subscription c1 <- subscribe input (notify listener)
  Subscription c2 <- subscribe output k
  pure (Subscription (c1 *> c2))
  where
  SubscribeIO { emitter, listener } = unsafePerformIO create
  (input, output) = f emitter
  -}
