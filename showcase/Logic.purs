-- | **The checkout app's business logic, as one inlined pipeline of optics.**
-- |
-- | No UI, no effects, no carrier: `p` stays abstract, so the logic is
-- | carrier-independent (see `doc/row-profunctors.md`). The four optics map onto
-- | Domain-Driven Design's tactical vocabulary, and each is one stage of the flow:
-- |
-- |   * **Lens**    (× → ×) — value-object field access  ("has-a")    — normalize fields
-- |   * **Shutter** (× → +) — the Process / Saga                       — lift fields to channels
-- |   * **Prism**   (+ → +) — value-object case match     ("is-a")     — route channels
-- |   * **Reel**    (+ → ×) — the Entity / Aggregate                   — render channels to notes
-- |
-- | The optics are inlined into `checkoutFlow`. Each carries an inline type annotation
-- | (`:: p (…) (…)`, using the signature's in-scope `p`) — that single-row signature is
-- | what lets the merge solve how to split the form across the two leaves.
module Showcase.Logic where

import Prelude

import Data.Either (Either(..), either)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, editProperty, lensProperty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, shutter, shutterE)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord (class Retaining, class VariantToRecord, reelE)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant, editCase, prismCase)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))

-- The model: just `Record`s (value objects) and, downstream, `Variant`s (channels).

-- | Money in minor units (cents) — a primitive value object.
type Money = Int

-- | The input **form** (a value-object record) and the rendered **display** (its projection).
type Form = { email :: String, amount :: Money }
type Display = { contactNote :: String, chargeNote :: String }

-- | The whole checkout, as one `Semigroupoid.do` (`>>>`) pipeline. Each merge do-block
-- | merges that stage's two optics (applied to the trivial focus `identity`), and the
-- | outer `Semigroupoid.do` flows the four stages together:
-- |
-- | ```
-- |   Form  ──Lens──▶  Form  ──Shutter──▶  Variant  ──Prism──▶  Variant  ──Reel──▶  Display
-- |   RecordToRecord.do    RecordToVariant.do     VariantToVariant.do     VariantToRecord.do
-- | ```
-- |
-- | Two axes of composition at once: **merge** across a row (inside each do-block) and
-- | **flow** along the pipeline (the outer `Semigroupoid.do`). The focuses are `identity`,
-- | so the app needs no parameters — the optics build all the structure.
checkoutFlow
  :: forall p
   . Category p
  => Strong p
  => Choice p
  => Resolving p
  => Retaining p
  => RecordToRecord p
  => RecordToVariant p
  => VariantToVariant p
  => VariantToRecord p
  => p Form Display
checkoutFlow = Semigroupoid.do
  RecordToRecord.do      -- × → ×   Lens: normalize each field
    (editProperty @"email" identity :: p (Record ( email :: String )) (Record ( email :: String )))
    (lensProperty @"amount" identity :: p (Record ( amount :: Money )) (Record ( amount :: Money )))
  RecordToVariant.do     -- × → +   Shutter: lift each field into a channel
    ( shutterE (\r -> Tuple r.email r.email) (either (inj (Proxy @"contact")) (inj (Proxy @"contact"))) identity
        :: p (Record ( email :: String )) (Variant ( contact :: String )) )
    ( shutter _.amount (inj (Proxy @"charge")) (\r -> inj (Proxy @"charge") r.amount) identity
        :: p (Record ( amount :: Money )) (Variant ( charge :: Money )) )
  VariantToVariant.do    -- + → +   Prism: route each channel
    (editCase @"contact" identity :: p (Variant ( contact :: String )) (Variant ( contact :: String )))
    (prismCase @"charge" identity :: p (Variant ( charge :: Money )) (Variant ( charge :: Money )))
  VariantToRecord.do     -- + → ×   Reel: render each channel into a note
    ( reelE (\v -> Left ((case_ # on (Proxy @"contact") identity) v)) (\(Tuple b (_ :: Unit)) -> { contactNote: b }) identity
        :: p (Variant ( contact :: String )) (Record ( contactNote :: String )) )
    ( reelE (\v -> Left ((case_ # on (Proxy @"charge") identity) v)) (\(Tuple m (_ :: Unit)) -> { chargeNote: show m }) identity
        :: p (Variant ( charge :: Money )) (Record ( chargeNote :: String )) )
