-- | Business-domain `Shutter`s and `Reel`s built via the **existential encoding**
-- | (`shutterE`/`reelE`). The whole point: the residual `c` is a *named business
-- | type* — not the collapsed `c := s` (`shutter`) or `c := b → t` (`reel`).
-- |
-- |   Shutter  ∃c. (s → a × c) × (b + c → t)   -- process focus a, keeping context c;
-- |                                               finish with the result b (Done) OR escape with c (Loop)
-- |   Reel     ∃c. (s → a + c) × (b × c → t)   -- fresh focus a OR resumed state c;
-- |                                               fold the result b into the carried state c
module BusinessOptics where

import Prelude

import Data.Either (Either(..))
import Data.Profunctor.Row.RecordToVariant (Shutter, shutterE, shutterWrap)
import Data.Profunctor.Row.VariantToRecord (Reel, reelE, reelWrap)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Type.Proxy (Proxy(..))

type Money = Int

--------------------------------------------------------------------------------
-- Shutter (× → +): run a focus, or snap shut by escaping with the retained c.
--------------------------------------------------------------------------------

-- | Submit an order, or escape to a saved draft. The residual `c` is the
-- | `DraftId` we keep aside while the submission runs.
-- |   s = OrderForm   a = Array String (items)   b = Confirmation
-- |   c = DraftId     t = OrderResult
type DraftId = String
type Confirmation = { ref :: String }
type OrderForm = { items :: Array String, draftId :: DraftId }
data OrderResult = Placed Confirmation | SavedDraft DraftId

submitOrder :: Shutter OrderForm OrderResult (Array String) Confirmation
submitOrder =
  shutterE
    (\form -> Tuple form.items form.draftId)   -- focus the items, retain the DraftId
    ( case _ of
        Left confirmation -> Placed confirmation   -- Done : the inner submit produced a Confirmation
        Right draftId     -> SavedDraft draftId )  -- Loop : escape, keep the draft

-- | KYC verification, or escape carrying the partial profile. Here `c` is the
-- | `PartialProfile` accumulated so far — genuinely distinct from `s`.
-- |   s = { document, partial }   a = Document   b = VerifiedIdentity
-- |   c = PartialProfile          t = KycResult
type Document = { kind :: String, number :: String }
type PartialProfile = { name :: String, fieldsLeft :: Int }
type VerifiedIdentity = { id :: String }
data KycResult = Verified VerifiedIdentity | Pending PartialProfile

verifyKyc
  :: Shutter { document :: Document, partial :: PartialProfile } KycResult Document VerifiedIdentity
verifyKyc =
  shutterE
    (\k -> Tuple k.document k.partial)   -- focus the document, retain the partial profile
    ( case _ of
        Left identity_ -> Verified identity_
        Right partial  -> Pending partial )

-- | `shutterWrap` (row, sub-Record focus): price an order line, focusing only the
-- | `(item, qty)` sub-Record; the leftover field `note` is **wrapped** into the
-- | output case `draft`. On `Done` the inner's `priced` case passes through; on
-- | `Loop` the unpriced remainder `{ note }` is carried out as `draft`, not dropped.
-- | (`Proxy @"draft"` is the only thing the caller must supply — the wrapper case name.)
checkout
  :: Shutter
       (Record (item :: String, qty :: Int, note :: String))          -- i'  full input
       (Variant (priced :: Money, draft :: Record (note :: String)))  -- o'  full output (o + case `draft`)
       (Record (item :: String, qty :: Int))                          -- i   sub-Record focus
       (Variant (priced :: Money))                                    -- o   inner output
checkout = shutterWrap (Proxy @"draft")

--------------------------------------------------------------------------------
-- Reel (+ → ×): fresh focus, or resume state c; fold the result into c.
--------------------------------------------------------------------------------

-- | Shopping cart. The residual `c` is the whole `Cart` carried forward — the
-- | retained aggregate. `recon` literally folds the priced line into it.
-- |   s = CartMsg   a = Item   b = PricedLine   c = Cart   t = Cart
type Item = { sku :: String }
type PricedLine = { sku :: String, price :: Money }
type Cart = { lines :: Array PricedLine, total :: Money }
data CartMsg = AddItem Item | RestoreCart Cart

cartReel :: Reel CartMsg Cart Item PricedLine
cartReel =
  reelE
    ( case _ of
        AddItem item  -> Left item     -- fresh focus: price this item
        RestoreCart c -> Right c )      -- resume: this Cart is the retained state
    (\(Tuple line cart) ->
        cart { lines = cart.lines <> [ line ], total = cart.total + line.price })

-- | Account ledger. The residual `c` is the running `Balance`; each posted entry
-- | is folded into it. A Mealy step over a transaction stream.
-- |   s = LedgerMsg   a = Transaction   b = PostedEntry   c = Balance   t = Balance
type Transaction = { amount :: Money }
type PostedEntry = { delta :: Money }
type Balance = { cents :: Money }
data LedgerMsg = Post Transaction | ResumeBalance Balance

ledgerReel :: Reel LedgerMsg Balance Transaction PostedEntry
ledgerReel =
  reelE
    ( case _ of
        Post tx         -> Left tx
        ResumeBalance b -> Right b )
    (\(Tuple entry balance) -> { cents: balance.cents + entry.delta })

-- | `reelWrap` (row, sub-Variant focus): the dual of `shutterWrap`. Focus the
-- | `cancel` sub-Variant; the rest of the input variant is wrapped into output
-- | field `pending`. (`Proxy @"pending"` is the only caller-supplied bit.)
countdownStep
  :: Reel
       (Variant (cancel :: Unit, tick :: Int))                       -- i'  full input
       (Record (done :: Boolean, pending :: Variant (tick :: Int)))  -- o'  full output (o + field `pending`)
       (Variant (cancel :: Unit))                                    -- i   sub-Variant focus
       (Record (done :: Boolean))                                    -- o   inner output
countdownStep = reelWrap (Proxy @"pending")
