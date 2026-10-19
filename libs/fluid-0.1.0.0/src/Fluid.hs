
{-# LANGUAGE CPP                        #-}

module Fluid
    ( FluidIO(..)
    , module Fluid.Component
    , module Fluid.Utils.Slot
    , module Data.Row
    , module Fluid.Query
    , module Fluid.Html
    , module Control.Monad.State.Class
    , module Data.Text
    )
where
import Fluid.Component
        ( ComponentSlot
        , ComponentSpec(..), Component, mkComponent, mkComponent'
        , EvalSpec(..), defaultEval, mkEval
        )
import Fluid.Query
        ( EvalM, Sink
        , scheduleSub, scheduleIO, scheduleIO_
        , scheduleSync, scheduleSync_
        , raise
        )
import Control.Monad.State.Class
import Fluid.Html
        ( Html, ComponentHtml
        )
import Fluid.Utils.Slot
        ( Slot
        , VoidF
        )
import Data.Row
        ( Row, type (.==), type (.+)
        )
import Data.Text
        ( Text
        , pack, unpack
        )

import qualified Fluid.Subscription as FS

-- | A record produced when the root component in a Fluid UI has been run.
-- |
-- | - `query` allows external sources to query the root component
-- | - `messages` allows external consumers to receive messages raised by the
-- |   root component
-- | - `dispose` stops running the UI and finalizes the root component
data FluidIO output = FluidIO
  { messages :: FS.Emitter output
  , dispose :: IO ()
  }

