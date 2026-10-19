
module Fluid.Utils.StableNamed
where
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName

---------------------------------
-- Object with a stable name (for fast equality)

data StableNamed a
    = StableNamed !a !(StableName a)

mkStableNamed :: a -> StableNamed a
mkStableNamed x = unsafePerformIO $ do
    sname <- x `seq` makeStableName x
    pure $ StableNamed x sname

unStableNamed :: StableNamed a -> a
unStableNamed (StableNamed x _) = x

instance Eq (StableNamed a) where
    StableNamed _ sn1 == StableNamed _ sn2 =
        sn1 == sn2

