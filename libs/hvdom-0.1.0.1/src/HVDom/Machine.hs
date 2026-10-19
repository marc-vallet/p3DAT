
module HVDom.Machine
  ( Machine
  , Step (..)
  , extract
  , step
  , halt
  )
where
import Data.Profunctor

type Machine a b = a -> IO (Step a b)

data Step a b = forall s. Step b s (s -> a -> IO (Step a b)) (s -> IO ())

{-# INLINE extract #-}
extract :: Step a b -> b
extract (Step r _ _ _) = r

{-# INLINE step #-}
step :: Step a b -> a -> IO (Step a b)
step (Step _ s stp _) = stp s

{-# INLINE halt #-}
halt :: Step a b -> IO ()
halt (Step _ s _ h) = h s


instance Functor (Step a) where
  -- fmap :: (b -> b') -> Step a b -> Step a b'
  fmap f (Step b s n h) =
    let n' s' a = fmap (fmap f) (n s' a)
    in Step (f b) s n' h

instance Profunctor Step where
  -- dimap :: (a -> a') -> (b -> b') -> Step a' b -> Step a b'
  dimap f g (Step b s n h) =
    let n' s' a = fmap (dimap f g) (n s' (f a))
    in Step (g b) s n' h

