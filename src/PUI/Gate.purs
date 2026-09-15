-- | The knowledge gate as a **pure Mealy machine** — the one piece of the
-- | carrier the record-output laws are really about, separated from the
-- | `Ref`s and callbacks that run it so that it can be reasoned about,
-- | enumerated and ported verbatim. This module imports no `Effect`:
-- | everything in it is a total function over a small state.
-- |
-- | One machine serves every product-shaped output the carrier gates. Its
-- | **participants** are keys — the owned *field labels* of a record merge
-- | (`recordToRecord`, `variantToRecord`), the element *keys* of the
-- | container action (`acted`) — each holding one slot, and the gate
-- | releases the whole in participant order once every participant is
-- | known, retaining last-known values thereafter. So the gather gate is
-- | the record gate with the row's labels supplied at runtime, and the
-- | zero-field clause is not a configuration but a consequence: an operand
-- | owning no field enrols no participant, so its emissions are no
-- | contribution — `{}` is always known — and it can neither open the gate
-- | nor starve a sibling. Fed `[]`, the collection has no participant left
-- | unknown and releases `[]`.
-- |
-- | The machine is **data-independent with finite control**. Its control is
-- | the participant sequence, which participants are known, how deep inside
-- | a broadcast step it is, and whether a contribution landed during the
-- | step; its data (the retained slots) is stored and re-emitted but never
-- | inspected — no branch of `gateStep` looks at a payload, and `v` is
-- | polymorphic to say so. That is what makes exhaustive checking complete
-- | rather than approximate (doc/observational-semantics.md, "The gate as a
-- | Mealy machine"): two machines built from this step either agree on every
-- | script or disagree on one shorter than the product of their
-- | control-state counts, and a fresh token per event is the most
-- | distinguishing environment there is. `known ⊆ order` is invariant.
-- |
-- | **Inputs.** `Contributed` — an operand emitted, as the slots it fills
-- | (a record merge trims the emission to its declared row and lists its
-- | fields; an element contributes its one slot); keys that are not
-- | participants are dropped, and a contribution filling no participant is
-- | no contribution. `Rekeyed` — the participant sequence changed (the
-- | collection was fed a new key set): survivors keep their slots, entrants
-- | are unknown, leavers are forgotten — and knowledge changed, so it lands
-- | like a contribution. `StepBegun`/`StepEnded` bracket one feed's
-- | broadcast — "a feed is one step" (Data.Profunctor.Row): contributions
-- | landing inside the brackets are held and released once at `StepEnded`,
-- | so a feed changing several fields never emits a torn row and a
-- | reconcile whose elements echo never gathers a half-updated vector.
-- | Steps nest (a re-entrant feed during a release), and only the outermost
-- | `StepEnded` releases.
-- |
-- | **Outputs.** `Released` — every participant known, the slots go
-- | downstream in participant order (a record merge assembles its row from
-- | them, the collection its array). `Withheld` — knowledge changed but some
-- | participants have never spoken: the emission is dropped, not delayed
-- | (the named `Strong` deviation), and the missing keys are handed back so
-- | the runner can name what the gate waits for. `Quiet` — nothing to say.
module PUI.Gate
  ( GateState
  , GateInput(..)
  , GateOutput(..)
  , initialGate
  , gateStep
  ) where

import Prelude

import Data.Array (filter) as Array
import Data.Foldable (elem, foldl)
import Data.Map (Map)
import Data.Map (empty, filterKeys, insert, lookup, member) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | The control state plus the retained slots: `order` is the participant
-- | sequence, `known` the slots that have spoken (always a subset of it).
type GateState k v =
  { order :: Array k
  , known :: Map k v
  , depth :: Int
  , pending :: Boolean
  }

data GateInput k v
  = Contributed (Array (Tuple k v))
  | Rekeyed (Array k)
  | StepBegun
  | StepEnded

data GateOutput k v
  = Released (Array (Tuple k v))
  | Withheld (Array k)
  | Quiet

derive instance (Eq k, Eq v) => Eq (GateOutput k v)

instance (Show k, Show v) => Show (GateOutput k v) where
  show = case _ of
    Released kvs -> "(Released " <> show kvs <> ")"
    Withheld ks -> "(Withheld " <> show ks <> ")"
    Quiet -> "Quiet"

-- | The state before any input: the participants enrolled, none known. A
-- | record merge enrols its owned labels; a collection enrols nothing and
-- | is rekeyed by its first feed.
initialGate :: forall k v. Array k -> GateState k v
initialGate order = { order, known: Map.empty, depth: 0, pending: false }

-- | One transition. Total, and the only place the gate's behaviour is
-- | decided.
gateStep
  :: forall k v
   . Ord k
  => GateState k v
  -> GateInput k v
  -> Tuple (GateState k v) (GateOutput k v)
gateStep s = case _ of
  Contributed kvs ->
    case Array.filter (\(Tuple k _) -> participant k) kvs of
      [] -> Tuple s Quiet
      mine -> landed s { known = foldl (\m (Tuple k v) -> Map.insert k v m) s.known mine }
  Rekeyed order ->
    landed s { order = order, known = Map.filterKeys (_ `elem` order) s.known }
  StepBegun -> Tuple s { depth = s.depth + 1 } Quiet
  StepEnded
    | s.depth > 1 -> Tuple s { depth = s.depth - 1 } Quiet
    | s.pending -> release s { depth = 0, pending = false }
    | otherwise -> Tuple s { depth = 0 } Quiet
  where
  participant k = k `elem` s.order
  -- knowledge that changed inside a step is held for the step's single
  -- release; outside one it releases at once
  landed s'
    | s'.depth > 0 = Tuple s' { pending = true } Quiet
    | otherwise = release s'

release
  :: forall k v
   . Ord k
  => GateState k v
  -> Tuple (GateState k v) (GateOutput k v)
release s = Tuple s case traverse (\k -> Tuple k <$> Map.lookup k s.known) s.order of
  Just kvs -> Released kvs
  Nothing -> Withheld (Array.filter (\k -> not (Map.member k s.known)) s.order)
