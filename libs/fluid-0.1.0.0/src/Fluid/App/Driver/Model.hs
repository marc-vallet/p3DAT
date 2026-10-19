
module Fluid.App.Driver.Model
where
import Fluid
import Fluid.App.Driver.View
import Fluid.Component
import Fluid.Utils.Conc
import Fluid.Utils.Slot as Slot
import Fluid.Utils.StableNamed


data DriverModel state action slots input output = DriverModel
        -- Camps invariants:
        { dmEq :: Maybe (state -> state -> Bool)
        , dmComponent :: ComponentSpec state action slots input output
        , dmKey :: ViewKey
        , dmOutputHandler :: output -> IO ()
        , dmFreshKey :: STM ViewKey
        -- Camps variants:
        , dmSeqNum :: TVar Int
        , dmState :: TVar (StableNamed state)
        , dmChildren :: TVar (SlotStorage slots DriverModelX)
        }

data DriverModelX i o = forall s act ps. DriverModelX (DriverModel s act ps i o)

unDriverModelX :: DriverModelX i o
               -> (forall s act ps. DriverModel s act ps i o -> a)
               -> a
unDriverModelX (DriverModelX dm) k = k dm


initDriverModel :: STM ViewKey
                -> Maybe (state -> state -> Bool)
                -> ComponentSpec state action slots input output
                -> input
                -> (output -> IO ())
                -> STM (DriverModel state action slots input output)
initDriverModel freshKey mbeq comp input ohandler = do
    key <- freshKey
    snref <- newTVar 0
    stref <- newTVar $ mkStableNamed $ initialState comp input
    childref <- newTVar Slot.empty
    pure DriverModel
        { dmEq = mbeq
        , dmComponent = comp
        , dmKey = key
        , dmOutputHandler = ohandler
        , dmFreshKey = freshKey
        , dmSeqNum = snref
        , dmState = stref
        , dmChildren = childref
        }

