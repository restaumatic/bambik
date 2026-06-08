-- | **The checkout app's business logic, as optics — and the app that composes them.**
-- |
-- | No UI, no effects, no carrier: `p` stays abstract, so the logic is
-- | carrier-independent (see `doc/row-profunctors.md`). The four optics map onto
-- | Domain-Driven Design's tactical vocabulary:
-- |
-- |   * **Lens**    (× → ×) — value-object field access  ("has-a")
-- |   * **Shutter** (× → +) — the Process / Saga           (run a step that finishes or loops)
-- |   * **Prism**   (+ → +) — value-object case match     ("is-a")
-- |   * **Reel**    (+ → ×) — the Entity / Aggregate       (fold a command into state)
-- |
-- | Each section defines the two **flow leaves** its family contributes; `checkoutFlow`
-- | (at the bottom) composes all eight with the four merge do-blocks and `Semigroupoid.do`.
-- | Every binding here is used by the app — there is no spare vocabulary.
module Showcase.Logic where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Lens, Prism)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, editProperty, lensProperty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, Shutter, shutter, shutterE)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord (class Retaining, class VariantToRecord, Reel, reelE)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant, editCase, prismCase)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import QualifiedDo.Semigroupoid as Semigroupoid
import Showcase.Domain (Display, Form, Money)
import Type.Proxy (Proxy(..))

-- ════════════════════════════════════════════════════════════════════════════
-- 1. LENS — value-object navigation ("has-a"). The diagonal × → ×.
--    Stage 1: normalize each form field in place.
-- ════════════════════════════════════════════════════════════════════════════

-- | `editProperty` — the in-place field lens.
_email :: Lens (Record ( email :: String )) (Record ( email :: String )) String String
_email = editProperty @"email"

-- | `lensProperty` — the (here monomorphic) row field lens.
_amount :: Lens (Record ( amount :: Money )) (Record ( amount :: Money )) Money Money
_amount = lensProperty @"amount"

-- ════════════════════════════════════════════════════════════════════════════
-- 2. SHUTTER — the Process / Saga. Mixed × → +: read a field, emit a channel case.
--    Stage 2: lift each field into a variant channel.
-- ════════════════════════════════════════════════════════════════════════════

-- | `shutterE` — built from the existential encoding (`decon` × `recon`). `Done` and
-- | `Loop` both land in the `contact` case here.
toContact :: Shutter (Record ( email :: String )) (Variant ( contact :: String )) String String
toContact =
  shutterE
    (\r -> Tuple r.email r.email)
    (either (inj (Proxy @"contact")) (inj (Proxy @"contact")))

-- | `shutter` — the explicit `(view, build, escape)` form.
toCharge :: Shutter (Record ( amount :: Money )) (Variant ( charge :: Money )) Money Money
toCharge =
  shutter
    _.amount
    (inj (Proxy @"charge"))
    (\r -> inj (Proxy @"charge") r.amount)

-- ════════════════════════════════════════════════════════════════════════════
-- 3. PRISM — value-object case discrimination ("is-a"). The diagonal + → +.
--    Stage 3: route each channel case in place.
-- ════════════════════════════════════════════════════════════════════════════

-- | `editCase` — the in-place case prism.
_contact :: Prism (Variant ( contact :: String )) (Variant ( contact :: String )) String String
_contact = editCase @"contact"

-- | `prismCase` — the (here monomorphic) row case prism.
_charge :: Prism (Variant ( charge :: Money )) (Variant ( charge :: Money )) Money Money
_charge = prismCase @"charge"

-- ════════════════════════════════════════════════════════════════════════════
-- 4. REEL — the Entity / Aggregate. Mixed + → ×: dispatch a case into a field.
--    Stage 4: render each channel into a display note.
-- ════════════════════════════════════════════════════════════════════════════

-- | `reelE` — built from the existential encoding: dispatch the case (`decon`), fill
-- | the output field (`recon`).
contactNote :: Reel (Variant ( contact :: String )) (Record ( contactNote :: String )) String String
contactNote =
  reelE
    (\v -> Left ((case_ # on (Proxy @"contact") identity) v))
    (\(Tuple b (_ :: Unit)) -> { contactNote: b })

chargeNote :: Reel (Variant ( charge :: Money )) (Record ( chargeNote :: String )) Money Money
chargeNote =
  reelE
    (\v -> Left ((case_ # on (Proxy @"charge") identity) v))
    (\(Tuple m (_ :: Unit)) -> { chargeNote: show m })

-- ════════════════════════════════════════════════════════════════════════════
-- 5. THE APP — `checkoutFlow` composes the eight flow leaves above.
-- ════════════════════════════════════════════════════════════════════════════

-- | The whole checkout, built **from the optics above**. Each merge do-block merges
-- | one family's two flow leaves (applied to the trivial focus `identity`), and the
-- | outer `Semigroupoid.do` flows the four stages together:
-- |
-- | ```
-- |   Form  ──Lens──▶  Form  ──Shutter──▶  Variant  ──Prism──▶  Variant  ──Reel──▶  Display
-- |   RecordToRecord.do    RecordToVariant.do     VariantToVariant.do     VariantToRecord.do
-- | ```
-- |
-- | Two axes of composition at once: **merge** across a row (inside each do-block,
-- | combining the two field/case leaves) and **flow** along the pipeline (the outer
-- | `Semigroupoid.do`). The four optic families are exactly the four stages, and the
-- | focuses are the trivial `identity`, so the app needs no parameters.
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
    _email identity
    _amount identity
  RecordToVariant.do     -- × → +   Shutter: lift each field into a channel
    toContact identity
    toCharge identity
  VariantToVariant.do    -- + → +   Prism: route each channel
    _contact identity
    _charge identity
  VariantToRecord.do     -- + → ×   Reel: render each channel into a note
    contactNote identity
    chargeNote identity
