
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Fluid.Utils.NT where

-- Natural transformation
newtype m ~> n = NT {runNT :: forall a. m a -> n a}

