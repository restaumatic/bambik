-- | The output gate of the two record-output merges (`recordToRecord`,
-- | `variantToRecord`) as a **pure Mealy machine** — the one piece of the
-- | carrier the merge laws are really about, separated from the `Ref`s and
-- | callbacks that run it so that it can be reasoned about, enumerated and
-- | ported verbatim. This module imports no `Effect`: everything in it is a
-- | total function over a small state.
-- |
-- | The machine is **data-independent with finite control**. Its control is
-- | four things — whether each side has spoken, how deep inside a broadcast
-- | step it is, and whether a contribution landed during the step — and its
-- | data (the two retained contributions) is stored and re-emitted but never
-- | inspected: no branch of `gateStep` looks at a payload. That is what makes
-- | exhaustive checking complete rather than approximate (doc/observational-
-- | semantics.md, "The gate as a Mealy machine"): two machines built from
-- | this step either agree on every script or disagree on one shorter than
-- | the product of their control-state counts, and a fresh token per event is
-- | the most distinguishing environment there is.
-- |
-- | **Inputs.** `Contributed1`/`Contributed2` — an operand emitted (already
-- | trimmed to its declared row by the caller: the type says exactly what the
-- | machine may see). `StepBegun`/`StepEnded` bracket one feed's broadcast —
-- | "a feed is one step" (Data.Profunctor.Row): contributions landing inside
-- | the brackets are held and released once at `StepEnded`, so a feed
-- | changing several fields never emits a torn row. Steps nest (a re-entrant
-- | feed during a release), and only the outermost `StepEnded` releases.
-- |
-- | **Outputs.** `Released` — both sides known, the left-biased union goes
-- | downstream (immaterial bias: the rows are disjoint). `Withheld1`/
-- | `Withheld2` — one side spoke, the other never has: the emission is
-- | dropped, not delayed (the named `Strong` deviation), and the value is
-- | handed back so the runner can name the starving sibling. `Quiet` —
-- | nothing to say.
-- |
-- | **Configuration.** A side owning zero fields (`owns = false`) is born
-- | satisfied — `{}` is always known — and its contributions are not
-- | contributions: they neither open the gate nor re-fire it. `initialGate`
-- | takes the two primes so that this module needs no coercion of its own.
module PUI.Gate
  ( GateConfig
  , GateState
  , GateInput(..)
  , GateOutput(..)
  , initialGate
  , gateStep
  , bothKnown
  ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Prim.Row (class Union)
import Record (union) as Record

-- | Which sides own at least one field. A side owning none is inert.
type GateConfig = { owns1 :: Boolean, owns2 :: Boolean }

-- | The control state plus the two retained contributions.
type GateState o1 o2 =
  { last1 :: Maybe { | o1 }
  , last2 :: Maybe { | o2 }
  , depth :: Int
  , pending :: Boolean
  }

data GateInput o1 o2
  = Contributed1 { | o1 }
  | Contributed2 { | o2 }
  | StepBegun
  | StepEnded

data GateOutput o1 o2 o
  = Released { | o }
  | Withheld1 { | o1 }
  | Withheld2 { | o2 }
  | Quiet

derive instance (Eq { | o1 }, Eq { | o2 }, Eq { | o }) => Eq (GateOutput o1 o2 o)

instance (Show { | o1 }, Show { | o2 }, Show { | o }) => Show (GateOutput o1 o2 o) where
  show = case _ of
    Released o -> "(Released " <> show o <> ")"
    Withheld1 a -> "(Withheld1 " <> show a <> ")"
    Withheld2 b -> "(Withheld2 " <> show b <> ")"
    Quiet -> "Quiet"

-- | The state before any input: each side unknown, or pre-known as the
-- | caller's prime when it owns no field.
initialGate :: forall o1 o2. Maybe { | o1 } -> Maybe { | o2 } -> GateState o1 o2
initialGate prime1 prime2 = { last1: prime1, last2: prime2, depth: 0, pending: false }

-- | Both sides have spoken (or were born satisfied).
bothKnown :: forall o1 o2. GateState o1 o2 -> Boolean
bothKnown s = isJust s.last1 && isJust s.last2

-- | One transition. Total, and the only place the gate's behaviour is
-- | decided.
gateStep
  :: forall o1 o2 o
   . Union o1 o2 o
  => GateConfig
  -> GateState o1 o2
  -> GateInput o1 o2
  -> Tuple (GateState o1 o2) (GateOutput o1 o2 o)
gateStep cfg s = case _ of
  Contributed1 v
    | not cfg.owns1 -> Tuple s Quiet
    | otherwise -> landed s { last1 = Just v }
  Contributed2 v
    | not cfg.owns2 -> Tuple s Quiet
    | otherwise -> landed s { last2 = Just v }
  StepBegun -> Tuple s { depth = s.depth + 1 } Quiet
  StepEnded
    | s.depth > 1 -> Tuple s { depth = s.depth - 1 } Quiet
    | s.pending -> release s { depth = 0, pending = false }
    | otherwise -> Tuple s { depth = 0 } Quiet
  where
  -- a contribution inside a step is held for the step's single release;
  -- outside one it releases at once
  landed s'
    | s'.depth > 0 = Tuple s' { pending = true } Quiet
    | otherwise = release s'

release
  :: forall o1 o2 o
   . Union o1 o2 o
  => GateState o1 o2
  -> Tuple (GateState o1 o2) (GateOutput o1 o2 o)
release s = Tuple s case s.last1, s.last2 of
  Just a, Just b -> Released (Record.union a b)
  Just a, Nothing -> Withheld1 a
  Nothing, Just b -> Withheld2 b
  Nothing, Nothing -> Quiet
