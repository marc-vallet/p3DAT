
module Fluid.Utils.Slot
where
import Prelude hiding (lookup)
import Data.Foldable (for_)
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Row
import qualified Data.Set as S
import Data.Typeable
import Control.Monad (guard)
import GHC.TypeLits (sameSymbol, symbolVal)


data VoidF p

data Slot (input :: Type) (output :: Type) (slotIndex :: Type)

-- some element of the `slots` row type, ordered by label and slot Ord instance
data SlotElem (slots :: Row Type) (slot :: Type -> Type -> Type) =
    forall sym input output sindex.
        (HasType sym (Slot input output sindex) slots, KnownSymbol sym, Ord sindex) =>
            SlotElem (Label sym) !sindex (slot input output)

instance Eq (SlotElem slots slot) where
  SlotElem l1 i1 _ == SlotElem l2 i2 _ =
    case sameSymbol l1 l2 of
      Just Refl -> i1 == i2
      Nothing -> False

instance Ord (SlotElem slots slot) where
  SlotElem l1 i1 _ `compare` SlotElem l2 i2 _ =
    case sameSymbol l1 l2 of
      Just Refl -> i1 `compare` i2
      Nothing -> symbolVal l1 `compare` symbolVal l2

newtype SlotStorage slots slot = SlotStorage (S.Set (SlotElem slots slot))
    deriving Eq

empty :: SlotStorage slots slot
empty = SlotStorage S.empty

lookup
  :: (HasType sym (Slot input output sindex) slots, KnownSymbol sym, Ord sindex)
  => Label sym
  -> sindex
  -> SlotStorage slots slot
  -> Maybe (slot input output)
lookup label index (SlotStorage set) = do
  SlotElem label' index' slot <- S.lookupGE (SlotElem label index undefined) set
  Refl <- sameSymbol label label'
  guard (index == index')
  Just slot

pop
  :: (HasType sym (Slot input output sindex) slots, KnownSymbol sym, Ord sindex)
  => Label sym
  -> sindex
  -> SlotStorage slots slot
  -> Maybe (slot input output, SlotStorage slots slot)
pop label index store@(SlotStorage set) = do
  slot <- lookup label index store
  Just (slot, SlotStorage $ S.delete (SlotElem label index slot) set)

insert
  :: (HasType sym (Slot input output sindex) slots, KnownSymbol sym, Ord sindex)
  => Label sym
  -> sindex -> slot input output
  -> SlotStorage slots slot
  -> SlotStorage slots slot
insert label index slot (SlotStorage set) =
  SlotStorage $ S.insert (SlotElem label index slot) set

slots
  :: (HasType sym (Slot input output sindex) slots, KnownSymbol sym, Ord sindex)
  => Label sym
  -> SlotStorage slots slot
  -> M.Map sindex (slot input output)
slots label (SlotStorage set) =
  M.fromAscList $ mapMaybe f $ S.toAscList set
  where
    f (SlotElem label' index' slot) = do -- Monad Maybe
      Refl <- sameSymbol label label'
      Just (index', slot)

foreachSlot
  :: forall slots slot m. (Applicative m)
  => SlotStorage slots slot
  -> (forall input output. slot input output -> m ())
  -> m ()
foreachSlot (SlotStorage s) act =
  for_ s f
  where
    f :: SlotElem slots slot -> m ()
    f (SlotElem _label _index slot) =
      --Refl <- sameSymbol label label'
      act slot

