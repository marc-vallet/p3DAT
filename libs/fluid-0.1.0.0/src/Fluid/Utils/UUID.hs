
module Fluid.Utils.UUID
    ( UUID, generateV4)
where
import Fluid.Random

import Data.UUID.Types (UUID, fromWords64)
import Control.Monad.Trans (MonadIO)

generateV4 :: MonadIO m => m UUID
generateV4 =
  fromWords64 <$> randomIO <*> randomIO

