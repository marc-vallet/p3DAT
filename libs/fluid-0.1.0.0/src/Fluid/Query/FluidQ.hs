
module Fluid.Query.FluidQ
where

data FluidQ action input
  = Initialize
  | Finalize
  | Receive input
  | Action action

showFluidQ :: FluidQ action input -> String
showFluidQ Initialize  = "Initialize"
showFluidQ Finalize    = "Finalize"
showFluidQ (Receive _) = "Receive"
showFluidQ (Action _)  = "Action"

