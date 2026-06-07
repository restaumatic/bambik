-- | **The entire business logic of the order app, expressed as optics — nothing else.**
-- |
-- | No UI, no effects, no carrier. Every binding is a pure optic: either a single
-- | optic value (`Lens`/`Prism`/`Reel`/`Shutter`) or a composite built with one of
-- | the four **merge do-blocks** — the `{Record, Variant} → {Record, Variant}` class
-- | matrix. The profunctor `p` stays abstract throughout: that is the whole point —
-- | one definition, carrier-independent (see `doc/row-profunctors.md`).
-- |
-- | The four single optics map onto Domain-Driven Design's tactical vocabulary:
-- |
-- |   * **Lens**    (× → ×) — value-object field access  ("has-a")
-- |   * **Prism**   (+ → +) — value-object case match     ("is-a")
-- |   * **Reel**    (+ → ×) — the Entity / Aggregate       (fold a command into state)
-- |   * **Shutter** (× → +) — the Process / Saga           (run a step that finishes or loops)
-- |
-- | The four merge do-blocks are how those compose into whole records and variants.
module Showcase.Logic where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Lens, Prism)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, class StrongRecordToRecord, editProperty, focusRecord, lensE, lensProperty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, Shutter, resolveProperty, shutter, shutterE, shutterWrap)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord (class Retaining, class VariantToRecord, Reel, reel, reelE, reelWrap, retainCase)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (class ChoiceVariantToVariant, class VariantToVariant, editCase, focusVariant, prismCase, prismE)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, on)
import Showcase.Domain (CardDetails, Customer, Line, Money, Order, OrderCmd, Payment, Submission(..))
import Type.Proxy (Proxy(..))

-- ════════════════════════════════════════════════════════════════════════════
-- 1. LENS — value-object navigation ("has-a"). The diagonal × → ×.
-- ════════════════════════════════════════════════════════════════════════════

-- | `editProperty` — the in-place field lens onto a value-object field.
_total :: Lens Order Order Money Money
_total = editProperty @"total"

-- | Lenses compose with `<<<`: navigate the aggregate down into a nested value object.
_city :: Lens Order Order String String
_city = editProperty @"customer" <<< editProperty @"address" <<< editProperty @"city"

-- | `lensProperty` — the *type-changing* field lens: the field's type may change.
_qty :: forall a b. Lens (Record ( sku :: String, qty :: a )) (Record ( sku :: String, qty :: b )) a b
_qty = lensProperty @"qty"

-- | `focusRecord` — row-typed `Strong`: focus a sub-record, carrying the rest.
onCustomer
  :: forall p
   . StrongRecordToRecord p
  => p (Record ( customer :: Customer )) (Record ( customer :: Customer ))
  -> p (Record ( ref :: String, customer :: Customer )) (Record ( ref :: String, customer :: Customer ))
onCustomer = focusRecord

-- | `lensE` — a `Lens` from its existential encoding (`decon` × `recon`).
-- | With `identity`/`identity` it is exactly `first`: the lens onto the first of a pair.
_fst :: forall a b c. Lens (Tuple a c) (Tuple b c) a b
_fst = lensE identity identity

-- ════════════════════════════════════════════════════════════════════════════
-- 2. PRISM — value-object case discrimination ("is-a"). The diagonal + → +.
-- ════════════════════════════════════════════════════════════════════════════

-- | `editCase` — the in-place case prism.
_card :: Prism Payment Payment CardDetails CardDetails
_card = editCase @"card"

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

-- | `prismE` — a `Prism` from its existential encoding. `identity`/`identity` = `_Left`.
_Left :: forall a b c. Prism (Either a c) (Either b c) a b
_Left = prismE identity identity

-- ════════════════════════════════════════════════════════════════════════════
-- 3. REEL — the Entity / Aggregate. Mixed + → ×: fold a command into state.
-- ════════════════════════════════════════════════════════════════════════════

-- | The **Order aggregate**: a `Reel` folding each command into the carried `Order`.
-- | `addLine` is a fresh focus (`Left`); `restore` rehydrates the state (`Right`).
-- | `recon` is the **aggregate root** — it re-establishes `total = Σ (price × qty)`
-- | on every transition.
orderAggregate :: Reel OrderCmd Order Line Line
orderAggregate = reelE decon recon
  where
  decon :: OrderCmd -> Either Line Order
  decon =
    case_
      # on (Proxy @"addLine") Left
      # on (Proxy @"restore") Right

  recon :: Tuple Line Order -> Order
  recon (Tuple line order) =
    order
      { lines = order.lines <> [ line ]
      , total = order.total + line.price * line.qty
      }

-- | `reel` — the co-Yoneda collapse (residual `c := b → t`): each input is a fresh
-- | focus (`Left`) or supplies a finisher drawn from retained state (`Right`).
counterReel :: Reel (Either Int (Int -> Int)) Int Int Int
counterReel = reel identity

-- | `retainCase` — the single-case edit-position combinator. Input case `status`
-- | resumes straight into output field `status`; other cases run the wrapped step.
resumeStatus
  :: forall p
   . Retaining p
  => p (Variant ( tick :: Int )) (Record ( done :: Boolean ))
  -> p (Variant ( status :: String, tick :: Int )) (Record ( status :: String, done :: Boolean ))
resumeStatus = retainCase @"status"

-- | `reelWrap` — row `Reel` focusing a sub-Variant; the rest of the input is wrapped
-- | into an output field. The dual of `priceLine`'s `shutterWrap`.
countdownStep
  :: Reel
       (Variant ( cancel :: Unit, tick :: Int ))
       (Record ( done :: Boolean, pending :: Variant ( tick :: Int ) ))
       (Variant ( cancel :: Unit ))
       (Record ( done :: Boolean ))
countdownStep = reelWrap (Proxy @"pending")

-- ════════════════════════════════════════════════════════════════════════════
-- 4. SHUTTER — the Process / Saga. Mixed × → +: run a step that finishes or loops.
-- ════════════════════════════════════════════════════════════════════════════

type CheckoutForm = { order :: Order, paid :: Boolean, draftId :: String }

-- | The **checkout process**: focus the decision data, retain the `draftId` as the
-- | residual. `Done` (`Left`) → `Placed`; `Loop` (`Right`) → escape to `SavedDraft`.
checkout :: Shutter CheckoutForm Submission { order :: Order, paid :: Boolean } { ref :: String }
checkout =
  shutterE
    (\f -> Tuple { order: f.order, paid: f.paid } f.draftId)
    (either Placed SavedDraft)

-- | `shutter` — the explicit `(view, build, escape)` form: a lens that can snap shut.
shipOrder :: Shutter Order Submission Order { ref :: String }
shipOrder = shutter identity Placed (\o -> SavedDraft o.ref)

-- | `resolveProperty` — the single-field edit-position combinator. Field `coupon`
-- | escapes directly to output case `coupon` (Loop), or the wrapped step runs (Done).
applyCoupon
  :: forall p
   . Resolving p
  => p (Record ( cart :: Order )) (Variant ( ok :: Money ))
  -> p (Record ( coupon :: String, cart :: Order )) (Variant ( ok :: Money, coupon :: String ))
applyCoupon = resolveProperty @"coupon"

-- | `shutterWrap` — row `Shutter` focusing a sub-Record; the rest is wrapped into an
-- | output case. On Loop the unpriced `{ note }` is carried out as `unpriced`.
priceLine
  :: Shutter
       (Record ( sku :: String, qty :: Int, note :: String ))
       (Variant ( priced :: Money, unpriced :: Record ( note :: String ) ))
       (Record ( sku :: String, qty :: Int ))
       (Variant ( priced :: Money ))
priceLine = shutterWrap (Proxy @"unpriced")

-- ════════════════════════════════════════════════════════════════════════════
-- 5. THE MERGE MATRIX — the four `Row → Row` do-blocks compose optics into whole
--    records and variants. Each merges complete sub-profunctors (the leaves).
-- ════════════════════════════════════════════════════════════════════════════

-- | **RecordToRecord.do** (× → ×) — assemble a record from field-producing leaves.
-- | Inputs *shared* (Inclusive), outputs *disjoint* (Exclusive).
orderSummary
  :: forall p
   . RecordToRecord p
  => p (Record ( ref :: String )) (Record ( label :: String ))
  -> p (Record ( total :: Money )) (Record ( formatted :: String ))
  -> p (Record ( ref :: String, total :: Money )) (Record ( label :: String, formatted :: String ))
orderSummary refLabel totalLabel = RecordToRecord.do
  refLabel
  totalLabel

-- | **RecordToVariant.do** (× → +) — the *form → event* shape: read a shared form
-- | (Inclusive in), merge the emitted events (Inclusive out).
validateCheckout
  :: forall p
   . RecordToVariant p
  => p (Record ( email :: String )) (Variant ( emailError :: String ))
  -> p (Record ( total :: Money )) (Variant ( emptyCartError :: String ))
  -> p (Record ( email :: String, total :: Money )) (Variant ( emailError :: String, emptyCartError :: String ))
validateCheckout vEmail vCart = RecordToVariant.do
  vEmail
  vCart

-- | **VariantToVariant.do** (+ → +) — dispatch the live input case (Exclusive in),
-- | merge outputs (Inclusive out — both branches may emit `authorized`).
routePayment
  :: forall p
   . VariantToVariant p
  => p (Variant ( card :: CardDetails )) (Variant ( authorized :: String ))
  -> p (Variant ( cash :: Money )) (Variant ( authorized :: String ))
  -> p (Variant ( card :: CardDetails, cash :: Money )) (Variant ( authorized :: String ))
routePayment onCard onCash = VariantToVariant.do
  onCard
  onCash

-- | **VariantToRecord.do** (+ → ×) — the *event → display* shape: dispatch on which
-- | response occurred (Exclusive in), fill disjoint display fields (Exclusive out).
renderResponse
  :: forall p
   . VariantToRecord p
  => p (Variant ( placed :: String )) (Record ( banner :: String ))
  -> p (Variant ( failed :: String )) (Record ( retry :: Boolean ))
  -> p (Variant ( placed :: String, failed :: String )) (Record ( banner :: String, retry :: Boolean ))
renderResponse onPlaced onFailed = VariantToRecord.do
  onPlaced
  onFailed
