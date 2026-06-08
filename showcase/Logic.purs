-- | **The order app's business logic, as optics — and one app that composes them.**
-- |
-- | No UI, no effects, no carrier: `p` stays abstract, so the logic is
-- | carrier-independent (see `doc/row-profunctors.md`). The four optics map onto
-- | Domain-Driven Design's tactical vocabulary:
-- |
-- |   * **Lens**    (× → ×) — value-object field access  ("has-a")
-- |   * **Prism**   (+ → +) — value-object case match     ("is-a")
-- |   * **Reel**    (+ → ×) — the Entity / Aggregate       (fold a command into state)
-- |   * **Shutter** (× → +) — the Process / Saga           (run a step that finishes or loops)
-- |
-- | Each section defines the **flow leaf** its family contributes to the app
-- | (`checkoutFlow`, at the bottom), plus more of that family's vocabulary. The app
-- | composes the four flow leaves with the four merge do-blocks and `Semigroupoid.do`.
module Showcase.Logic where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Lens, Prism)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, class StrongRecordToRecord, editProperty, focusRecord, lensE, lensProperty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, Shutter, resolveProperty, shutter, shutterE, shutterWrap)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord (class Retaining, class VariantToRecord, Reel, reel, reelE, reelWrap, retainCase)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class ChoiceVariantToVariant, class VariantToVariant, editCase, focusVariant, prismCase, prismE)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import QualifiedDo.Semigroupoid as Semigroupoid
import Showcase.Domain (CardDetails, Customer, Money, Order, Submission(..))
import Type.Proxy (Proxy(..))

-- ════════════════════════════════════════════════════════════════════════════
-- 1. LENS — value-object navigation ("has-a"). The diagonal × → ×.
-- ════════════════════════════════════════════════════════════════════════════

-- | Flow leaves — `editProperty`, the in-place field lens. Applied to a focus they
-- | become the × → × stage of `checkoutFlow`.
_email :: Lens (Record ( email :: String )) (Record ( email :: String )) String String
_email = editProperty @"email"

_amount :: Lens (Record ( amount :: Money )) (Record ( amount :: Money )) Money Money
_amount = editProperty @"amount"

-- More of the Lens vocabulary:

-- | `lensProperty` — the *type-changing* field lens (the field's type may change).
_qty :: forall a b. Lens (Record ( sku :: String, qty :: a )) (Record ( sku :: String, qty :: b )) a b
_qty = lensProperty @"qty"

-- | `focusRecord` — row-typed `Strong`: focus a sub-record, carry the rest.
onCustomer
  :: forall p
   . StrongRecordToRecord p
  => p (Record ( customer :: Customer )) (Record ( customer :: Customer ))
  -> p (Record ( ref :: String, customer :: Customer )) (Record ( ref :: String, customer :: Customer ))
onCustomer = focusRecord

-- | `lensE` — a `Lens` from its existential encoding; `identity`/`identity` = `first`.
_fst :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_fst = lensE identity identity

-- ════════════════════════════════════════════════════════════════════════════
-- 2. PRISM — value-object case discrimination ("is-a"). The diagonal + → +.
-- ════════════════════════════════════════════════════════════════════════════

-- | Flow leaves — `editCase`, the in-place case prism. The + → + stage of the app.
_emailEvt :: Prism (Variant ( emailEvt :: String )) (Variant ( emailEvt :: String )) String String
_emailEvt = editCase @"emailEvt"

_amountEvt :: Prism (Variant ( amountEvt :: Money )) (Variant ( amountEvt :: Money )) Money Money
_amountEvt = editCase @"amountEvt"

-- More of the Prism vocabulary:

-- | `prismCase` — the *type-changing* case prism: re-tag a case, changing its payload.
_cash :: forall a b. Prism (Variant ( cash :: a, card :: CardDetails )) (Variant ( cash :: b, card :: CardDetails )) a b
_cash = prismCase @"cash"

-- | `focusVariant` — row-typed `Choice`: focus a sub-variant, carry the other cases.
onCardCase
  :: forall p
   . ChoiceVariantToVariant p
  => p (Variant ( card :: CardDetails )) (Variant ( card :: CardDetails ))
  -> p (Variant ( card :: CardDetails, cash :: Money )) (Variant ( card :: CardDetails, cash :: Money ))
onCardCase = focusVariant

-- | `prismE` — a `Prism` from its existential encoding; `identity`/`identity` = `_Left`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = prismE identity identity

-- ════════════════════════════════════════════════════════════════════════════
-- 3. SHUTTER — the Process / Saga. Mixed × → +: run a step that finishes or loops.
-- ════════════════════════════════════════════════════════════════════════════

-- | Flow leaves — `shutterE`: read a field, emit an event case. The × → + stage.
-- | (`Done` and `Loop` both land in the event case here; a richer process would
-- | split them — see `shipOrder` below.)
emailEvent :: Shutter (Record ( email :: String )) (Variant ( emailEvt :: String )) String String
emailEvent =
  shutterE
    (\r -> Tuple r.email r.email)
    (either (inj (Proxy @"emailEvt")) (inj (Proxy @"emailEvt")))

amountEvent :: Shutter (Record ( amount :: Money )) (Variant ( amountEvt :: Money )) Money Money
amountEvent =
  shutterE
    (\r -> Tuple r.amount r.amount)
    (either (inj (Proxy @"amountEvt")) (inj (Proxy @"amountEvt")))

-- More of the Shutter vocabulary:

-- | `shutter` — the explicit `(view, build, escape)` form: run the focus and `build`
-- | (Done), or `escape` straight to the output (Loop).
shipOrder :: Shutter Order Submission Order { ref :: String }
shipOrder = shutter identity Placed (\o -> SavedDraft { draftId: o.ref })

-- | `resolveProperty` — single-field edit-position: field `coupon` escapes to output
-- | case `coupon` (Loop), or the wrapped step runs (Done).
applyCoupon
  :: forall p
   . Resolving p
  => p (Record ( cart :: Order )) (Variant ( ok :: Money ))
  -> p (Record ( coupon :: String, cart :: Order )) (Variant ( ok :: Money, coupon :: String ))
applyCoupon = resolveProperty @"coupon"

-- | `shutterWrap` — row `Shutter` focusing a sub-Record; the rest is wrapped into an
-- | output case (on Loop the unpriced `{ note }` is carried out as `unpriced`).
priceLine
  :: Shutter
       (Record ( sku :: String, qty :: Int, note :: String ))
       (Variant ( priced :: Money, unpriced :: Record ( note :: String ) ))
       (Record ( sku :: String, qty :: Int ))
       (Variant ( priced :: Money ))
priceLine = shutterWrap (Proxy @"unpriced")

-- ════════════════════════════════════════════════════════════════════════════
-- 4. REEL — the Entity / Aggregate. Mixed + → ×: fold a command into state.
-- ════════════════════════════════════════════════════════════════════════════

-- | Flow leaves — `reelE`: dispatch the event case into an output display field.
-- | The + → × stage of the app.
emailNote :: Reel (Variant ( emailEvt :: String )) (Record ( emailNote :: String )) String String
emailNote =
  reelE
    (\v -> Left ((case_ # on (Proxy @"emailEvt") identity) v))
    (\(Tuple b (_ :: Unit)) -> { emailNote: b })

amountNote :: Reel (Variant ( amountEvt :: Money )) (Record ( amountNote :: String )) Money Money
amountNote =
  reelE
    (\v -> Left ((case_ # on (Proxy @"amountEvt") identity) v))
    (\(Tuple m (_ :: Unit)) -> { amountNote: show m })

-- More of the Reel vocabulary:

-- | `reel` — the co-Yoneda collapse (residual `c := b → t`): a fresh focus (`Left`)
-- | or a finisher drawn from retained state (`Right`).
counterReel :: Reel (Either Int (Int -> Int)) Int Int Int
counterReel = reel identity

-- | `retainCase` — single-case edit-position: input case `status` resumes into output
-- | field `status`; other cases run the wrapped step.
resumeStatus
  :: forall p
   . Retaining p
  => p (Variant ( tick :: Int )) (Record ( done :: Boolean ))
  -> p (Variant ( status :: String, tick :: Int )) (Record ( status :: String, done :: Boolean ))
resumeStatus = retainCase @"status"

-- | `reelWrap` — row `Reel` focusing a sub-Variant; the rest is wrapped into an
-- | output field. The dual of `priceLine`'s `shutterWrap`.
countdownStep
  :: Reel
       (Variant ( cancel :: Unit, tick :: Int ))
       (Record ( done :: Boolean, pending :: Variant ( tick :: Int ) ))
       (Variant ( cancel :: Unit ))
       (Record ( done :: Boolean ))
countdownStep = reelWrap (Proxy @"pending")

-- ════════════════════════════════════════════════════════════════════════════
-- 5. THE APP — `checkoutFlow` composes the four flow leaves above.
-- ════════════════════════════════════════════════════════════════════════════

-- | The whole checkout, built **from the optics above**. Each merge do-block merges
-- | one family's flow leaves (applied to the trivial focus `identity`), and the outer
-- | `Semigroupoid.do` flows the four stages together:
-- |
-- | ```
-- |   Record  ──Lens──▶  Record  ──Shutter──▶  Variant  ──Prism──▶  Variant  ──Reel──▶  Record
-- |    RecordToRecord.do      RecordToVariant.do     VariantToVariant.do     VariantToRecord.do
-- | ```
-- |
-- | Two axes of composition at once: **merge** across a row (inside each do-block,
-- | combining the two field/case leaves) and **flow** along the pipeline (the outer
-- | `Semigroupoid.do`). The four optic families are exactly the four stages.
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
  => p (Record ( email :: String, amount :: Money )) (Record ( emailNote :: String, amountNote :: String ))
checkoutFlow = Semigroupoid.do
  RecordToRecord.do      -- × → ×   Lens: normalize each field
    _email identity
    _amount identity
  RecordToVariant.do     -- × → +   Shutter: turn each field into an event
    emailEvent identity
    amountEvent identity
  VariantToVariant.do    -- + → +   Prism: route each event case
    _emailEvt identity
    _amountEvt identity
  VariantToRecord.do     -- + → ×   Reel: render each event into a display field
    emailNote identity
    amountNote identity
